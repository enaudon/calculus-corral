module Id = Identifier
module Loc = Location

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

let to_type =
  let rec to_type tp_bvs env tm = match tm.desc with
    | Variable id ->
      begin try Id.Map.find id env with
        | Id.Unbound id ->
          error tm.loc "to_type" @@
            Printf.sprintf "undefined identifier '%s'" (Id.to_string id)
      end
    | Term_abs (arg, arg_tp, body) ->
      let body_tp = to_type tp_bvs (Id.Map.add arg arg_tp env) body in
      Type.func arg_tp body_tp
    | Term_app (fn, arg) ->
      let fn' = to_type tp_bvs env fn in
      let fml_arg_tp, res_tp =
        try
          Type.get_func fn'
        with Invalid_argument _ ->
          error tm.loc "to_type" @@
            Printf.sprintf
              "expected function type; found '%s'"
              (Type.to_string fn')
      in
      let act_arg_tp = to_type tp_bvs env arg in
      if Type.alpha_equivalent act_arg_tp fml_arg_tp then
        res_tp
      else
        error arg.loc "to_type" @@
            Printf.sprintf
              "expected type '%s'; found type '%s'"
              (Type.to_string fml_arg_tp)
              (Type.to_string act_arg_tp)
    | Type_abs (arg, body) ->
      let tp_bvs' = Id.Set.add arg tp_bvs in
      let tv = Type.var @@ Id.to_string arg in
      Type.forall (Id.to_string arg) @@
        to_type tp_bvs' (Id.Map.add arg tv env) body
    | Type_app (fn, arg) ->
      let fn' = to_type tp_bvs env fn in
      let tv, tp =
        try
          Type.get_forall fn'
        with Invalid_argument _ ->
          error tm.loc "to_type" @@
            Printf.sprintf
              "expected universal type; found '%s'"
              (Type.to_string fn')
      in
      Type.subst tp_bvs (Id.Map.singleton tv arg) tp
  in
  to_type Id.Set.empty Id.Map.empty

(* Transformations *)

(** [free_vars tm] computes the free term variables in [tm]. *)
let free_vars : t -> Id.Set.t =
  let rec free_vars fvs tm = match tm.desc with
    | Variable id -> Id.Set.add id fvs
    | Term_abs (arg, _, body) -> Id.Set.del arg @@ free_vars fvs body
    | Type_abs (_, body) -> free_vars fvs body
    | Term_app (fn, arg) -> free_vars (free_vars fvs fn) arg
    | Type_app (fn, _) -> free_vars fvs fn
  in
  free_vars Id.Set.empty

(**
  [subst_tp ~fvs tm id tp'] replaces occurences of [id] in [tm] with
  [tp'].  The optional argument, [fvs], is a set of identifiers that may
  occur free in [tp'].

  [subst_tp] avoids name capture by renaming binders in [tp] to follow
  the Barendregt convention--i.e. the names of bound variable are chosen
  distinct from those of free variables.  The set [fvs] is used for this
  purpose: while traversing [tp], [subst_tp] renames any identifiers
  which are members of [fvs], and therefore may occur free in [tp'].
 *)
let subst_tp : ?fvs : Id.Set.t -> t -> Id.t -> Type.t -> t =
    fun ?fvs tm id tp' ->
  let rec subst fvs sub tm =
    let loc = tm.loc in
    match tm.desc with
      | Variable _ ->
        tm
      | Term_abs (arg, tp, body) ->
        abs loc arg
          (Type.subst fvs (Id.Map.singleton id tp') tp)
          (subst fvs sub body)
      | Term_app (fn, arg) ->
        app loc (subst fvs sub fn) (subst fvs sub arg)
      | Type_abs (arg, body) when Id.Set.mem arg fvs ->
        let arg' = Id.fresh () in
        let sub' = Id.Map.add arg (Type.var (Id.to_string arg')) sub in
        tp_abs loc arg' @@ subst (Id.Set.add arg' fvs) sub' body
      | Type_abs (arg, body) ->
        tp_abs loc arg @@
          subst (Id.Set.add arg fvs) (Id.Map.del arg sub) body
      | Type_app (fn, arg) ->
        tp_app loc (subst fvs sub fn) (Type.subst fvs sub arg)
  in
  let fvs = match fvs with
    | None -> Type.free_vars tp'
    | Some fvs -> fvs
  in
  subst fvs (Id.Map.singleton id tp') tm

(**
  [subst_tm ~fvs tm id tm'] replaces occurences of [id] in [tm] with
  [tm'].  The optional argument, [fvs], is a set of identifiers that may
  occur free in [tm'].

  As with [subst_tp], [subst_tm] avoids name capture by following the
  Barendregt convention.
 *)
let subst_tm : ?fvs : Id.Set.t -> t -> Id.t -> t -> t =
    fun ?fvs tm id tm' ->
  let rec subst fvs sub tm =
    let loc = tm.loc in
    match tm.desc with
      | Variable id ->
        Id.Map.find_default tm id sub
      | Term_abs (arg, tp, body) when Id.Set.mem arg fvs ->
        let arg' = Id.fresh () in
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
  let fvs = match fvs with
    | None -> free_vars tm'
    | Some fvs -> fvs
  in
  subst fvs (Id.Map.singleton id tm') tm

(**
  [beta_reduce tm] beta-reduces [tm].

  In order to make calls to [subst] more efficient, [beta_reduce] tracks
  bound variables as it traverses [tm].  These bound variables may occur
  free in sub-expressions of [tm], such as the argument in function
  applications.  Therefore, they become the free variables passed to
  [subst].  Doing this, means [subst] can skip the call to [free_vars],
  saving an extra traversal over the [tm'] argument to [subst].

  This technique is due to Jones, et al.:
    Peyton Jones, Simon; Marlow, Simon. Secrets of the Glasgow Haskell
    Compiler inliner.  Journal of Functional Programming, 2002.
 *)
let beta_reduce ?deep =
  let deep = if deep = None then false else true in
  let rec beta_reduce tp_bvs tm_bvs tm =
    let loc = tm.loc in
    match tm.desc with
      | Variable id ->
        if Id.Set.mem id tm_bvs then
          tm
        else
          error tm.loc "beta_reduce" @@
            Printf.sprintf
              "undefined identifier '%s'"
              (Id.to_string id)
      | Term_abs (arg, tp, body) ->
        if deep then
          abs loc arg tp @@
            beta_reduce tp_bvs (Id.Set.add arg tm_bvs) body
        else
          tm
      | Term_app (fn, act_arg) ->
        let fn' = beta_reduce tp_bvs tm_bvs fn in
        let act_arg' = beta_reduce tp_bvs tm_bvs act_arg in
        begin match fn'.desc with
          | Term_abs (fml_arg, _, body) ->
            let body' = subst_tm ~fvs:tm_bvs body fml_arg act_arg' in
            beta_reduce tp_bvs tm_bvs body'
          | _ ->
            app loc fn' act_arg'
        end
      | Type_abs (arg, body) ->
        if deep then
          tp_abs loc arg @@ beta_reduce (tp_bvs) tm_bvs body
        else
          tm
      | Type_app (fn, act_arg) ->
        let fn' = beta_reduce tp_bvs tm_bvs fn in
        begin match fn'.desc with
          | Type_abs (fml_arg, body) ->
            let body' = subst_tp ~fvs:tp_bvs body fml_arg act_arg in
            beta_reduce tp_bvs tm_bvs body'
          | _ ->
            tp_app loc fn' act_arg
        end
  in
  beta_reduce Id.Set.empty Id.Set.empty

(* Utilities *)

let alpha_equivalent =
  let rec alpha_equiv tp_env tm_env tm1 tm2 =
    match tm1.desc, tm2.desc with
      | Variable id1, Variable id2 ->
        let id1' = try Id.Map.find id1 tm_env with
          | Id.Unbound id ->
            error tm1.loc "alpha_equivalent" @@
              Printf.sprintf
                "undefined identifier '%s'"
                (Id.to_string id)
        in
        id1' = id2
      | Term_abs (arg1, tp1, body1), Term_abs (arg2, tp2, body2) ->
        Type.alpha_equivalent ~env:tp_env tp1 tp2 &&
          alpha_equiv tp_env (Id.Map.add arg1 arg2 tm_env) body1 body2
      | Term_app (fn1, arg1), Term_app (fn2, arg2) ->
        alpha_equiv tp_env tm_env fn1 fn2 &&
          alpha_equiv tp_env tm_env arg1 arg2
      | Type_abs (arg1, body1), Type_abs (arg2, body2) ->
        alpha_equiv (Id.Map.add arg1 arg2 tp_env) tm_env body1 body2
      | Type_app (fn1, arg1), Type_app (fn2, arg2) ->
        alpha_equiv tp_env tm_env fn1 fn2 &&
          Type.alpha_equivalent ~env:tp_env arg1 arg2
      | _ ->
        false
  in
  alpha_equiv Id.Map.empty Id.Map.empty

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
      Printf.sprintf "%s : %s . %s"
        (Id.to_string arg)
        (Type.to_string tp)
        (to_string body)
    | Type_abs (arg, body) ->
      Printf.sprintf "%s . %s"
        (Id.to_string arg)
        (to_string body)
    | Term_app (fn, arg) ->
      let arg_to_string tm = match tm.desc with
        | Variable _ ->
          to_string tm
        | Term_abs _ | Term_app _ | Type_abs _ | Type_app _ ->
          to_paren_string tm
      in
      Printf.sprintf "%s %s" (fn_to_string fn) (arg_to_string arg)
    | Type_app (fn, arg) ->
      Printf.sprintf "%s %s" (fn_to_string fn) (Type.to_string arg)

(* Constructors *)

let var ?(loc = Loc.dummy) id = var loc (Id.of_string id)

let abs ?(loc = Loc.dummy) arg tp body =
  abs loc (Id.of_string arg) tp body

let abs' ?(loc = Loc.dummy) args body =
  let abs' body (arg, tp) = abs ~loc arg tp body in
  List.fold_left abs' body (List.rev args)

let app ?(loc = Loc.dummy) fn arg = app loc fn arg

let app' ?(loc = Loc.dummy) fn args =
  List.fold_left (fun fn args -> app ~loc fn args) fn args

let tp_abs ?(loc = Loc.dummy) arg body =
  tp_abs loc (Id.of_string arg) body

let tp_abs' ?(loc = Loc.dummy) args body =
  let tp_abs' body arg = tp_abs ~loc arg body in
  List.fold_left tp_abs' body (List.rev args)

let tp_app ?(loc = Loc.dummy) fn arg = tp_app loc fn arg

let tp_app' ?(loc = Loc.dummy) fn args =
  List.fold_left (fun fn args -> tp_app ~loc fn args) fn args