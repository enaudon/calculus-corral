module Id = Identifier
module Loc = Location
module Misc = Miscellaneous

type desc =
  | Variable of Id.t
  | Term_abs of Id.t * Type.t * t
  | Term_app of t * t
  | Type_abs of Id.t * t
  | Type_app of t * Type.t

and t = {
  desc : desc ;
  loc : Loc.t ;
}

(* Internal utilities *)

let error : Loc.t -> string -> string -> 'a = fun loc fn_name msg ->
  failwith @@
    Printf.sprintf "%s %s.%s: %s"
      (Loc.to_string loc)
      __MODULE__
      fn_name
      msg

let var : Loc.t -> Id.t -> t = fun loc id ->
  { desc = Variable id; loc }

let abs : Loc.t -> Id.t -> Type.t -> t -> t = fun loc arg tp body ->
  { desc = Term_abs (arg, tp, body); loc }

let app : Loc.t -> t -> t -> t = fun loc fn arg ->
  { desc = Term_app (fn, arg); loc }

let tp_abs : Loc.t -> Id.t -> t -> t = fun loc arg body ->
  { desc = Type_abs (arg, body); loc }

let tp_app : Loc.t -> t -> Type.t -> t = fun loc fn arg ->
  { desc = Type_app (fn, arg); loc }

(* Typing *)

let rec to_type ?(env = Id.Map.empty) tm =
  let to_type env = to_type ~env in
  match tm.desc with
    | Variable id ->
      begin try Id.Map.find id env with
        | Id.Unbound id ->
          error tm.loc "to_type" @@
            Printf.sprintf "undefined identifier '%s'" (Id.to_string id)
      end
    | Term_abs (arg, arg_tp, body) ->
      let body_tp = to_type (Id.Map.add arg arg_tp env) body in
      Type.func arg_tp body_tp
    | Term_app (fn, arg) ->
      let fn_tp = to_type env fn in
      let fml_arg_tp, res_tp =
        try
          Type.get_func (Type.beta_reduce ~deep:() ~env fn_tp)
        with Invalid_argument _ ->
          error tm.loc "to_type" @@
            Printf.sprintf
              "expected function type; found '%s'"
              (Type.to_string fn_tp)
      in
      let act_arg_tp = to_type env arg in
      if Type.alpha_equivalent ~beta_env:env act_arg_tp fml_arg_tp then
        res_tp
      else
        error arg.loc "to_type" @@
            Printf.sprintf
              "expected type '%s'; found type '%s'"
              (Type.to_string fml_arg_tp)
              (Type.to_string act_arg_tp)
    | Type_abs (arg, body) ->
      let env' = Id.Map.add arg (Type.var arg) env in
      Type.forall arg @@ to_type env' body
    | Type_app (fn, arg) ->
      let fn_tp = to_type env fn in
      let tv, tp =
        try
          Type.get_forall @@ Type.beta_reduce ~deep:() ~env fn_tp
        with Invalid_argument _ ->
          error tm.loc "to_type" @@
            Printf.sprintf
              "expected universal type; found '%s'"
              (Type.to_string fn_tp)
      in
      let sub = Id.Map.singleton tv arg in
      Type.subst (Id.Set.of_list @@ Id.Map.keys env) sub tp

(* Transformations *)

(** [free_vars tm] computes the free term variables in [tm]. *)
let free_vars : t -> Id.Set.t =
  let rec free_vars fvs tm = match tm.desc with
    | Variable id -> Id.Set.add id fvs
    | Term_abs (arg, _, body) -> Id.Set.del arg @@ free_vars fvs body
    | Term_app (fn, arg) -> free_vars (free_vars fvs fn) arg
    | Type_abs (_, body) -> free_vars fvs body
    | Type_app (fn, _) -> free_vars fvs fn
  in
  free_vars Id.Set.empty

(**
  [subst_tp tm id tp'] replaces occurences of [id] in [tm] with [tp'].

  [subst_tp] avoids name capture by renaming binders in [tp] to follow
  the Barendregt convention--i.e. the names of bound variable are chosen
  distinct from those of free variables.
 *)
let subst_tp : t -> Id.t -> Type.t -> t = fun tm id tp' ->
  let rec subst fvs sub tm =
    let loc = tm.loc in
    match tm.desc with
      | Variable _ ->
        tm
      | Term_abs (arg, tp, body) ->
        abs loc arg (Type.subst fvs sub tp) (subst fvs sub body)
      | Term_app (fn, arg) ->
        app loc (subst fvs sub fn) (subst fvs sub arg)
      | Type_abs (arg, body) when Id.Set.mem arg fvs ->
        let arg' = Id.fresh_upper () in
        let sub' = Id.Map.add arg (Type.var arg') sub in
        tp_abs loc arg' @@ subst (Id.Set.add arg' fvs) sub' body
      | Type_abs (arg, body) ->
        tp_abs loc arg @@
          subst (Id.Set.add arg fvs) (Id.Map.del arg sub) body
      | Type_app (fn, arg) ->
        tp_app loc (subst fvs sub fn) (Type.subst fvs sub arg)
  in
  subst (Type.free_vars tp') (Id.Map.singleton id tp') tm

(**
  [subst_tm tm id tm'] replaces occurences of [id] in [tm] with [tm'].

  As with [subst_tp], [subst_tm] avoids name capture by following the
  Barendregt convention.
 *)
let subst_tm : t -> Id.t -> t -> t = fun tm id tm' ->
  let rec subst fvs sub tm =
    let loc = tm.loc in
    match tm.desc with
      | Variable id ->
        Id.Map.find_default tm id sub
      | Term_abs (arg, tp, body) when Id.Set.mem arg fvs ->
        let arg' = Id.fresh_lower () in
        let sub' = Id.Map.add arg (var Loc.dummy arg') sub in
        abs loc arg' tp @@ subst (Id.Set.add arg' fvs) sub' body
      | Term_abs (arg, tp, body) ->
        abs loc arg tp @@
          subst (Id.Set.add arg fvs) (Id.Map.del arg sub) body
      | Term_app (fn, arg) ->
        app loc (subst fvs sub fn) (subst fvs sub arg)
      | Type_abs (arg, body) ->
         tp_abs loc arg @@ subst fvs sub body
      | Type_app (fn, arg) ->
        tp_app loc (subst fvs sub fn) arg
  in
  subst (free_vars tm') (Id.Map.singleton id tm') tm

let rec beta_reduce ?deep ?(env = Id.Map.empty) tm =
  let beta_reduce env = beta_reduce ?deep ~env in
  let loc = tm.loc in
  match tm.desc with
    | Variable id ->
      Id.Map.find_default tm id env
    | Term_abs (arg, tp, body) ->
      if deep <> None then
        let env' = Id.Map.del arg env in
        abs loc arg tp @@ beta_reduce env' body
      else
        tm
    | Term_app (fn, act_arg) ->
      let fn' = beta_reduce env fn in
      let act_arg' = beta_reduce env act_arg in
      begin match fn'.desc with
        | Term_abs (fml_arg, _, body) ->
          let body' = subst_tm body fml_arg act_arg' in
          let env' = Id.Map.del fml_arg env in
          beta_reduce env' body'
        | _ ->
          app loc fn' act_arg'
      end
    | Type_abs (arg, body) ->
      if deep <> None then
        let env' = Id.Map.del arg env in
        tp_abs loc arg @@ beta_reduce env' body
      else
        tm
    | Type_app (fn, act_arg) ->
      let fn' = beta_reduce env fn in
      match fn'.desc with
        | Type_abs (fml_arg, body) ->
          let body' = subst_tp body fml_arg act_arg in
          let env' = Id.Map.del fml_arg env in
          beta_reduce env' body'
        | _ ->
          tp_app loc fn' act_arg

(* Utilities *)

let alpha_equivalent =
  let rec alpha_equiv tp_env tm_env tm1 tm2 =
    match tm1.desc, tm2.desc with
      | Variable id1, Variable id2 ->
        Id.alpha_equivalent tm_env id1 id2
      | Term_abs (arg1, tp1, body1), Term_abs (arg2, tp2, body2) ->
        Type.alpha_equivalent ~env:tp_env tp1 tp2 &&
          alpha_equiv tp_env ((arg1, arg2) :: tm_env) body1 body2
      | Term_app (fn1, arg1), Term_app (fn2, arg2) ->
        alpha_equiv tp_env tm_env fn1 fn2 &&
          alpha_equiv tp_env tm_env arg1 arg2
      | Type_abs (arg1, body1), Type_abs (arg2, body2) ->
        alpha_equiv ((arg1, arg2) :: tp_env) tm_env body1 body2
      | Type_app (fn1, arg1), Type_app (fn2, arg2) ->
        alpha_equiv tp_env tm_env fn1 fn2 &&
          Type.alpha_equivalent ~env:tp_env arg1 arg2
      | _ ->
        false
  in
  alpha_equiv [] []

let simplify tm =

  let fresh =
    let cntr = ref (-1) in
    fun () ->
      incr cntr;
      Id.of_string @@ Misc.int_to_upper !cntr
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
      | Type_abs (arg, body) ->
        let arg' = fresh () in
        let env' = Id.Map.add arg (Type.var arg') env in
        let body' = simplify env' body in
        tp_abs loc arg' body'
      | Type_app (fn, arg) ->
        let fn' = simplify env fn in
        let arg' = Type.simplify ~ctx:(fresh, env) arg in
        tp_app loc fn' arg'
  in

  simplify Id.Map.empty tm

let rec to_string tm =
  let to_paren_string tm = Printf.sprintf "(%s)" (to_string tm) in
  let fn_to_string tm = match tm.desc with
    | Variable _ | Term_app _ | Type_app _ -> to_string tm
    | Term_abs _ | Type_abs _ -> to_paren_string tm
  in
  match tm.desc with
    | Variable id ->
      Id.to_string id
    | Term_abs (arg, tp, body) ->
      Printf.sprintf "\\%s : %s . %s"
        (Id.to_string arg)
        (Type.to_string tp)
        (to_string body)
    | Term_app (fn, arg) ->
      let arg_to_string tm = match tm.desc with
        | Variable _ ->
          to_string tm
        | Term_abs _ | Term_app _ | Type_abs _ | Type_app _ ->
          to_paren_string tm
      in
      Printf.sprintf "%s %s" (fn_to_string fn) (arg_to_string arg)
    | Type_abs (arg, body) ->
      Printf.sprintf "\\%s . %s"
        (Id.to_string arg)
        (to_string body)
    | Type_app (fn, arg) ->
      Printf.sprintf "%s %s" (fn_to_string fn) (Type.to_string arg)

(* Constructors *)

let var ?(loc = Loc.dummy) id = var loc id

let abs ?(loc = Loc.dummy) arg tp body = abs loc arg tp body

let abs' ?(loc = Loc.dummy) args body =
  let abs' body (arg, tp) = abs ~loc arg tp body in
  List.fold_left abs' body (List.rev args)

let app ?(loc = Loc.dummy) fn arg = app loc fn arg

let app' ?(loc = Loc.dummy) fn args =
  List.fold_left (fun fn args -> app ~loc fn args) fn args

let tp_abs ?(loc = Loc.dummy) arg body = tp_abs loc arg body

let tp_abs' ?(loc = Loc.dummy) args body =
  let tp_abs' body arg = tp_abs ~loc arg body in
  List.fold_left tp_abs' body (List.rev args)

let tp_app ?(loc = Loc.dummy) fn arg = tp_app loc fn arg

let tp_app' ?(loc = Loc.dummy) fn args =
  List.fold_left (fun fn args -> tp_app ~loc fn args) fn args
