module Id = Identifier
module Kind_env = Kind.Environment
module Loc = Location
module Misc = Miscellaneous
module Opt = Option
module Type_env = Type.Environment

type desc =
  | Variable of Id.t
  | Term_abs of Id.t * Type.t * t
  | Term_app of t * t
  | Type_abs of Id.t * Kind.t * t
  | Type_app of t * Type.t

and t =
  { desc : desc;
    loc : Loc.t }

module Env = Environment.Make (struct
  type value = t

  let initial = []
end)

(* Internal utilities *)

let error : Loc.t -> string -> string -> 'a =
 fun loc fn_name msg ->
   failwith
   @@ Printf.sprintf "%s %s.%s: %s" (Loc.to_string loc) __MODULE__ fn_name msg

let var : Loc.t -> Id.t -> t = fun loc id -> {desc = Variable id; loc}

let abs : Loc.t -> Id.t -> Type.t -> t -> t =
 fun loc arg tp body -> {desc = Term_abs (arg, tp, body); loc}

let app : Loc.t -> t -> t -> t =
 fun loc fn arg -> {desc = Term_app (fn, arg); loc}

let tp_abs : Loc.t -> Id.t -> Kind.t -> t -> t =
 fun loc arg kn body -> {desc = Type_abs (arg, kn, body); loc}

let tp_app : Loc.t -> t -> Type.t -> t =
 fun loc fn arg -> {desc = Type_app (fn, arg); loc}

(* Typing *)

let rec to_type (kn_env, tp_env) tm =
  let to_type kn_env tp_env = to_type (kn_env, tp_env) in
  match tm.desc with
    | Variable id ->
      ( try Type_env.Term.find id tp_env
        with Id.Unbound id ->
          error tm.loc "to_type"
          @@ Printf.sprintf "undefined identifier '%s'" (Id.to_string id) )
    | Term_abs (arg, arg_tp, body) ->
      let arg_kn = Type.to_kind kn_env arg_tp in
      if not (Kind.alpha_equivalent arg_kn Kind.prop) then
        error tm.loc "to_type"
        @@ Printf.sprintf
             "expected proper kind; found '%s'"
             (Kind.to_string arg_kn);
      let tp_env' = Type_env.Term.add arg arg_tp tp_env in
      Type.func arg_tp @@ to_type kn_env tp_env' body
    | Term_app (fn, arg) ->
      let fn_tp = to_type kn_env tp_env fn in
      let fml_arg_tp, res_tp =
        try Type.get_func (Type.reduce tp_env fn_tp)
        with Invalid_argument _ ->
          error tm.loc "to_type"
          @@ Printf.sprintf
               "expected function type; found '%s'"
               (Type.to_string fn_tp)
      in
      let act_arg_tp = to_type kn_env tp_env arg in
      let beta_env = tp_env in
      if Type.alpha_equivalent ~beta_env act_arg_tp fml_arg_tp then
        res_tp
      else
        error arg.loc "to_type"
        @@ Printf.sprintf
             "expected type '%s'; found type '%s'"
             (Type.to_string fml_arg_tp)
             (Type.to_string act_arg_tp)
    | Type_abs (arg, kn, body) ->
      Type.forall arg kn @@ to_type (Kind_env.add arg kn kn_env) tp_env body
    | Type_app (fn, arg) ->
      let fn_tp = to_type kn_env tp_env fn in
      let fn_quant, fn_kn, fn_body =
        try Type.get_forall @@ Type.reduce tp_env fn_tp
        with Invalid_argument _ ->
          error tm.loc "to_type"
          @@ Printf.sprintf
               "expected universal type; found '%s'"
               (Type.to_string fn_tp)
      in
      let arg_kn = Type.to_kind kn_env arg in
      if not (Kind.alpha_equivalent arg_kn fn_kn) then
        error tm.loc "to_type"
        @@ Printf.sprintf
             "expected kind '%s'; found '%s'"
             (Kind.to_string fn_kn)
             (Kind.to_string arg_kn);
      let ftvs = Id.Set.of_list @@ Kind_env.keys kn_env in
      Type.subst ftvs (Type_env.Type.singleton fn_quant arg) fn_body

(* Transformations *)

(* [subst_tp] avoids name capture by renaming binders in [tp] to follow the
   Barendregt convention--i.e. the names of bound variable are chosen distinct
   from those of free variables. *)
let rec subst_tp fvs sub tm =
  let loc = tm.loc in
  match tm.desc with
    | Variable _ ->
      tm
    | Term_abs (arg, tp, body) ->
      abs loc arg (Type.subst fvs sub tp) (subst_tp fvs sub body)
    | Term_app (fn, arg) ->
      app loc (subst_tp fvs sub fn) (subst_tp fvs sub arg)
    | Type_abs (arg, kn, body) when Id.Set.mem arg fvs ->
      let arg' = Id.gen_upper () in
      let sub' = Type_env.Type.add arg (Type.var arg') sub in
      tp_abs loc arg' kn @@ subst_tp (Id.Set.add arg' fvs) sub' body
    | Type_abs (arg, kn, body) ->
      tp_abs loc arg kn
      @@ subst_tp (Id.Set.add arg fvs) (Type_env.Type.del arg sub) body
    | Type_app (fn, arg) ->
      tp_app loc (subst_tp fvs sub fn) (Type.subst fvs sub arg)

(* As with [subst_tp], [subst_tm] avoids name capture by following the
   Barendregt convention. *)
let rec subst_tm fvs sub tm =
  let loc = tm.loc in
  match tm.desc with
    | Variable id ->
      Env.find_default tm id sub
    | Term_abs (arg, tp, body) when Id.Set.mem arg fvs ->
      let arg' = Id.gen_lower () in
      let sub' = Env.add arg (var Loc.dummy arg') sub in
      abs loc arg' tp @@ subst_tm (Id.Set.add arg' fvs) sub' body
    | Term_abs (arg, tp, body) ->
      abs loc arg tp @@ subst_tm (Id.Set.add arg fvs) (Env.del arg sub) body
    | Term_app (fn, arg) ->
      app loc (subst_tm fvs sub fn) (subst_tm fvs sub arg)
    | Type_abs (arg, kn, body) ->
      tp_abs loc arg kn @@ subst_tm fvs sub body
    | Type_app (fn, arg) ->
      tp_app loc (subst_tm fvs sub fn) arg

let rec beta_reduce ?deep (tp_env, tm_env) tm =
  let beta_reduce tp_env tm_env = beta_reduce ?deep (tp_env, tm_env) in
  let subst_tp env tm id tp =
    let fvs = Id.Set.of_list @@ Type_env.Type.keys env in
    subst_tp fvs (Type_env.Type.singleton id tp) tm
  in
  let subst_tm env tm id tm' =
    subst_tm (Id.Set.of_list @@ Env.keys env) (Env.singleton id tm') tm
  in
  let loc = tm.loc in
  match tm.desc with
    | Variable id ->
      Env.find_default tm id tm_env
    | Term_abs (arg, tp, body) ->
      if deep <> Opt.none then
        let tm_env' = Env.add arg (var Loc.dummy arg) tm_env in
        abs loc arg tp @@ beta_reduce tp_env tm_env' body
      else
        tm
    | Term_app (fn, act_arg) ->
      let fn' = beta_reduce tp_env tm_env fn in
      let act_arg' = beta_reduce tp_env tm_env act_arg in
      ( match fn'.desc with
        | Term_abs (fml_arg, _, body) ->
          let tm_env' = Env.del fml_arg tm_env in
          beta_reduce tp_env tm_env' @@ subst_tm tm_env body fml_arg act_arg'
        | _ ->
          app loc fn' act_arg' )
    | Type_abs (arg, kn, body) ->
      if deep <> Opt.none then
        let tp_env' = Type_env.Type.add arg (Type.var arg) tp_env in
        tp_abs loc arg kn @@ beta_reduce tp_env' tm_env body
      else
        tm
    | Type_app (fn, act_arg) ->
      let fn' = beta_reduce tp_env tm_env fn in
      ( match fn'.desc with
        | Type_abs (fml_arg, _, body) ->
          let tm_env' = Env.del fml_arg tm_env in
          beta_reduce tp_env tm_env' @@ subst_tp tp_env body fml_arg act_arg
        | _ ->
          tp_app loc fn' act_arg )

(* Utilities *)

let alpha_equivalent =
  let rec alpha_equiv tp_env tm_env tm1 tm2 =
    match (tm1.desc, tm2.desc) with
      | Variable id1, Variable id2 ->
        Id.alpha_equivalent tm_env id1 id2
      | Term_abs (arg1, tp1, body1), Term_abs (arg2, tp2, body2) ->
        Type.alpha_equivalent ~env:tp_env tp1 tp2
        && alpha_equiv tp_env ((arg1, arg2) :: tm_env) body1 body2
      | Term_app (fn1, arg1), Term_app (fn2, arg2) ->
        alpha_equiv tp_env tm_env fn1 fn2 && alpha_equiv tp_env tm_env arg1 arg2
      | Type_abs (arg1, kn1, body1), Type_abs (arg2, kn2, body2) ->
        Kind.alpha_equivalent kn1 kn2
        && alpha_equiv ((arg1, arg2) :: tp_env) tm_env body1 body2
      | Type_app (fn1, arg1), Type_app (fn2, arg2) ->
        alpha_equiv tp_env tm_env fn1 fn2
        && Type.alpha_equivalent ~env:tp_env arg1 arg2
      | _ ->
        false
  in
  alpha_equiv [] []

let simplify tm =
  let fresh =
    let cntr = ref (-1) in
    fun () ->
      incr cntr;
      Id.define @@ Misc.int_to_upper !cntr
  in
  let rec simplify env tm =
    let loc = tm.loc in
    match tm.desc with
      | Variable _ ->
        tm
      | Term_abs (arg, tp, body) ->
        let tp' = Type.simplify ~ctx:(fresh, env) tp in
        let body' = simplify env body in
        abs loc arg tp' body'
      | Term_app (fn, arg) ->
        let fn' = simplify env fn in
        let arg' = simplify env arg in
        app loc fn' arg'
      | Type_abs (arg, kn, body) when Id.is_generated arg ->
        let arg' = fresh () in
        let env' = Id.Map.add arg (Type.var arg') env in
        tp_abs loc arg' kn @@ simplify env' body
      | Type_abs (arg, kn, body) ->
        tp_abs loc arg kn @@ simplify env body
      | Type_app (fn, arg) ->
        let fn' = simplify env fn in
        let arg' = Type.simplify ~ctx:(fresh, env) arg in
        tp_app loc fn' arg'
  in
  simplify Id.Map.empty tm

let rec to_string tm =
  let to_paren_string tm = Printf.sprintf "(%s)" (to_string tm) in
  let fn_to_string tm =
    match tm.desc with
      | Variable _ | Term_app _ | Type_app _ ->
        to_string tm
      | Term_abs _ | Type_abs _ ->
        to_paren_string tm
  in
  let arg_to_string tm =
    match tm.desc with
      | Variable _ ->
        to_string tm
      | Term_abs _ | Term_app _ | Type_abs _ | Type_app _ ->
        to_paren_string tm
  in
  match tm.desc with
    | Variable id ->
      Id.to_string id
    | Term_abs (arg, tp, body) ->
      Printf.sprintf
        "\\%s : %s . %s"
        (Id.to_string arg)
        (Type.to_string tp)
        (to_string body)
    | Term_app (fn, arg) ->
      Printf.sprintf "%s %s" (fn_to_string fn) (arg_to_string arg)
    | Type_abs (arg, kn, body) ->
      Printf.sprintf
        "\\%s :: %s . %s"
        (Id.to_string arg)
        (Kind.to_string kn)
        (to_string body)
    | Type_app (fn, arg) ->
      Printf.sprintf "%s %s" (fn_to_string fn) (Type.to_string arg)

(* Containers *)

module Environment = Env

(* Constructors *)

let var ?(loc = Loc.dummy) id = var loc id

let abs ?(loc = Loc.dummy) arg tp body = abs loc arg tp body

let abs' ?(loc = Loc.dummy) args body =
  List.fold_right (fun (arg, tp) body -> abs ~loc arg tp body) args body

let app ?(loc = Loc.dummy) fn arg = app loc fn arg

let app' ?(loc = Loc.dummy) fn args = List.fold_left (app ~loc) fn args

let tp_abs ?(loc = Loc.dummy) arg kn body = tp_abs loc arg kn body

let tp_abs' ?(loc = Loc.dummy) args body =
  let tp_abs (arg, kn) body = tp_abs ~loc arg kn body in
  List.fold_right tp_abs args body

let tp_app ?(loc = Loc.dummy) fn arg = tp_app loc fn arg

let tp_app' ?(loc = Loc.dummy) fn args = List.fold_left (tp_app ~loc) fn args
