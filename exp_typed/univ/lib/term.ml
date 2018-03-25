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

let error : Loc.t -> string -> 'a = fun loc msg ->
  failwith @@ Printf.sprintf "%s %s" (Loc.to_string loc) msg

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
  let rec to_type btvs env tm = match tm.desc with
    | Variable id ->
      begin try Id.Map.find id env with
        | Id.Unbound id ->
          error tm.loc @@
            Printf.sprintf
              "Term.to_type: undefined identifier '%s'"
              (Id.to_string id)
      end
    | Term_abs (arg, arg_tp, body) ->
      let body_tp = to_type btvs (Id.Map.add arg arg_tp env) body in
      Type.func arg_tp body_tp
    | Term_app (fn, arg) ->
      let fml_arg_tp, res_tp =
        try
          Type.get_func @@ to_type btvs env fn
        with Invalid_argument _ ->
          error tm.loc @@
            Printf.sprintf
              "Term.to_type: cannot apply non-function"
      in
      let act_arg_tp = to_type btvs env arg in
      if Type.struct_equivalent act_arg_tp fml_arg_tp then
        res_tp
      else
        error arg.loc @@
            Printf.sprintf
              "Term.to_type: expected type '%s'; found type '%s'"
                (Type.to_string fml_arg_tp)
                (Type.to_string act_arg_tp)
    | Type_abs (arg, body) ->
      let btvs' = Id.Set.add arg btvs in
      let tv = Type.var @@ Id.to_string arg in
      Type.forall (Id.to_string arg) @@
        to_type btvs' (Id.Map.add arg tv env) body
    | Type_app (fn, arg) ->
        let tv, tp = try Type.get_forall @@ to_type btvs env fn with
          | Invalid_argument _ ->
            error tm.loc @@
              Printf.sprintf
                "Term.to_type: cannot apply non-function"
        in
        Type.subst btvs (Id.Map.singleton tv arg) tp
  in
  to_type Id.Set.empty Id.Map.empty

(* Transformations *)

(** [free_vars tm] computes the free term variables in [tm]. *)
let free_vars : t -> Id.Set.t =
  let rec free_vars fvars tm = match tm.desc with
    | Variable id ->
      Id.Set.add id fvars
    | Term_abs (arg, _, body) ->
      Id.Set.del arg @@ free_vars fvars body
    | Type_abs (_, body) ->
      free_vars fvars body
    | Term_app (fn, arg) ->
      free_vars (free_vars fvars fn) arg
    | Type_app (fn, _) ->
      free_vars fvars fn
  in
  free_vars Id.Set.empty

(**
  [subst_tp fvars tm id tp'] replaces occurences of [id] in [tm] with
  [tp'].  [fvars] is a set of identifiers that may occur free in [tp'].

  [subst_tp] avoids name capture by renaming binders in [tp] to follow
  the Barendregt convention--i.e. the names of bound variable are chosen
  distinct from those of free variables.  The set [fvars] is used for
  this purpose: while traversing [tp], [subst_tp] renames any
  identifiers which are members of [fvars], and therefore may occur free
  in [tp'].
 *)
let subst_tp : ?fvars : Id.Set.t -> t -> Id.t -> Type.t -> t =
    fun ?fvars tm id tp' ->
  let rec subst fvars sub tm =
    let loc = tm.loc in
    match tm.desc with
      | Variable _ ->
        tm
      | Term_abs (arg, tp, body) ->
        abs loc arg
          (Type.subst fvars (Id.Map.singleton id tp') tp)
          (subst fvars sub body)
      | Term_app (fn, arg) ->
        app loc (subst fvars sub fn) (subst fvars sub arg)
      | Type_abs (arg, body) ->
        if Id.Set.mem arg fvars then
          let arg' = Id.fresh () in
          let sub' = Id.Map.add arg (Type.var (Id.to_string arg')) sub in
          tp_abs loc arg' @@
            subst (Id.Set.add arg' fvars) sub' body
        else
          tp_abs loc arg @@
            subst (Id.Set.add arg fvars) (Id.Map.del arg sub) body
      | Type_app (fn, arg) ->
        tp_app loc (subst fvars sub fn) (Type.subst fvars sub arg)
  in
  let fvars = match fvars with
    | None -> Type.free_vars tp'
    | Some fvs -> fvs
  in
  subst fvars (Id.Map.singleton id tp') tm

(**
  [subst_tm ~fvars tm id tm'] replaces occurences of [id] in [tm] with
  [tm'].  The optional argument, [fvars], is a set of identifiers that
  may occur free in [tm'].

  As with [subst_tp], [subst_tm] avoids name capture by following the
  Barendregt convention.
 *)
let subst_tm : ?fvars : Id.Set.t -> t -> Id.t -> t -> t =
    fun ?fvars tm id tm' ->
  let rec subst fvars sub tm =
    let loc = tm.loc in
    match tm.desc with
      | Variable id ->
        Id.Map.find_default tm id sub
      | Term_abs (arg, tp, body) ->
        if Id.Set.mem arg fvars then
          let arg' = Id.fresh () in
          let sub' = Id.Map.add arg (var Loc.dummy arg') sub in
          abs loc arg' tp @@
            subst (Id.Set.add arg' fvars) sub' body
        else
          abs loc arg tp @@
            subst (Id.Set.add arg fvars) (Id.Map.del arg sub) body
      | Term_app (fn, arg) ->
        app loc (subst fvars sub fn) (subst fvars sub arg)
      | Type_abs (arg, body) ->
         tp_abs loc arg @@ subst fvars sub body
      | Type_app (fn, arg) ->
        tp_app loc (subst fvars sub fn) arg
  in
  let fvars = match fvars with
    | None -> free_vars tm'
    | Some fvs -> fvs
  in
  subst fvars (Id.Map.singleton id tm') tm

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
  let rec beta_reduce btvs bvs tm =
    let loc = tm.loc in
    match tm.desc with
      | Variable id ->
        if Id.Set.mem id bvs then
          tm
        else
          error tm.loc @@
            Printf.sprintf
              "Term.beta_reduce: undefined identifier '%s'"
              (Id.to_string id)
      | Term_abs (arg, tp, body) ->
        if deep then
          abs loc arg tp @@ beta_reduce btvs (Id.Set.add arg bvs) body
        else
          tm
      | Term_app (fn, act_arg) ->
        let fn' = beta_reduce btvs bvs fn in
        let act_arg' = beta_reduce btvs bvs act_arg in
        begin match fn'.desc with
          | Term_abs (fml_arg, _, body) ->
            let body' = subst_tm ~fvars:bvs body fml_arg act_arg' in
            beta_reduce btvs bvs body'
          | _ ->
            app loc fn' act_arg'
        end
      | Type_abs (arg, body) ->
        if deep then
          tp_abs loc arg @@ beta_reduce (btvs) bvs body
        else
          tm
      | Type_app (fn, act_arg) ->
        let fn' = beta_reduce btvs bvs fn in
        begin match fn'.desc with
          | Type_abs (fml_arg, body) ->
            let body' = subst_tp ~fvars:btvs body fml_arg act_arg in
            beta_reduce btvs bvs body'
          | _ ->
            tp_app loc fn' act_arg
        end
  in
  beta_reduce Id.Set.empty Id.Set.empty

(* Utilities *)

(**
  [struct_equivalent tp1 tp2] determines whether [tp1] and [tp2] are
  structurally equivalent.  [struct_equivalent] does the traversal
  manually, rather than delegating to [Pervasives.(=)], because terms
  contain source code locations, which it deliberately ignores.
 *)
let rec struct_equivalent tm1 tm2 = match tm1.desc, tm2.desc with
  | Variable id1, Variable id2 ->
    id1 = id2
  | Term_abs (arg1, tp1, body1), Term_abs (arg2, tp2, body2) ->
    arg1 = arg2 && Type.struct_equivalent tp1 tp2 &&
      struct_equivalent body1 body2
  | Term_app (fn1, arg1), Term_app (fn2, arg2) ->
    struct_equivalent fn1 fn2 && struct_equivalent arg1 arg2
  | Type_abs (arg1, body1), Type_abs (arg2, body2) ->
    arg1 = arg2 && struct_equivalent body1 body2
  | Type_app (fn1, arg1), Type_app (fn2, arg2) ->
    struct_equivalent fn1 fn2 && Type.struct_equivalent arg1 arg2
  | _ ->
    false

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
