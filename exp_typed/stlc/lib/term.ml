module Id = Identifier
module Loc = Location

type desc =
  | Variable of Id.t
  | Abstraction of Id.t * Type.t * t
  | Application of t * t

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
  { desc = Abstraction (arg, tp, body); loc }

let app : Loc.t -> t -> t -> t = fun loc fn arg ->
  { desc = Application (fn, arg); loc }

(* Typing *)

let to_type =
  let rec to_type env tm = match tm.desc with
    | Variable id ->
      begin try Id.Map.find id env with
        | Id.Unbound id ->
          error tm.loc @@
            Printf.sprintf
              "Term.to_type: undefined identifier '%s'"
              (Id.to_string id)
      end
    | Abstraction (arg, arg_tp, body) ->
      let body_tp = to_type (Id.Map.add arg arg_tp env) body in
      Type.func arg_tp body_tp
    | Application (fn, arg) ->
      let fml_arg_tp, res_tp = try Type.get_func @@ to_type env fn with
        | Invalid_argument _ ->
          error tm.loc @@
            Printf.sprintf
              "Term.to_type: cannot apply non-function"
      in
      let act_arg_tp = to_type env arg in
      if Type.struct_equivalent act_arg_tp fml_arg_tp then
        res_tp
      else
        error arg.loc @@
            Printf.sprintf
              "Term.to_type: expected type '%s'; found type '%s'"
                (Type.to_string fml_arg_tp)
                (Type.to_string act_arg_tp)
  in
  to_type Id.Map.empty

(* Transformations *)

(** [free_vars tm] computes the free variables in [tm]. *)
let free_vars : t -> Id.Set.t =
  let rec free_vars fvars tm = match tm.desc with
    | Variable id ->
      Id.Set.add id fvars
    | Abstraction (arg, _, body) ->
      Id.Set.del arg @@ free_vars fvars body
    | Application (fn, arg) ->
      free_vars (free_vars fvars fn) arg
  in
  free_vars Id.Set.empty

(**
  [subst ~fvars tm id tm'] replaces occurences of [id] in [tm] with
  [tm'].  The optional argument, [fvars], is a set of identifiers that
  may occur free in [tm'].

  [subst] avoids name capture by renaming binders in [tm] to follow the
  Barendregt convention--i.e. the names of bound variable are chosen
  distinct from those of free variables.  The set [fvars] is used for
  this purpose: while traversing [tm], [subst] renames any identifiers
  which are members of [fvars], and therefore may occur free in [tm'].
 *)
let subst : ?fvars : Id.Set.t -> t -> Id.t -> t -> t =
    fun ?fvars tm id tm' ->
  let rec subst fvars sub tm =
    let loc = tm.loc in
    match tm.desc with
      | Variable id ->
        Id.Map.find_default tm id sub
      | Abstraction (arg, tp, body) ->
        if Id.Set.mem arg fvars then
          let arg' = Id.fresh () in
          let sub' = Id.Map.add arg (var Loc.dummy arg') sub in
          abs loc arg' tp @@
            subst (Id.Set.add arg' fvars) sub' body
        else
          abs loc arg tp @@
            subst (Id.Set.add arg fvars) (Id.Map.del arg sub) body
      | Application (fn, arg) ->
        app loc (subst fvars sub fn) (subst fvars sub arg)
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
  let rec beta_reduce bvars tm =
    let loc = tm.loc in
    match tm.desc with
      | Variable id ->
        if Id.Set.mem id bvars then
          tm
        else
          error tm.loc @@
            Printf.sprintf
              "Term.beta_reduce: undefined identifier '%s'"
              (Id.to_string id)
      | Abstraction (arg, tp, body) ->
        if deep then
          abs loc arg tp @@ beta_reduce (Id.Set.add arg bvars) body
        else
          tm
      | Application (fn, act_arg) ->
        let fn' = beta_reduce bvars fn in
        let act_arg' = beta_reduce bvars act_arg in
        match fn'.desc with
          | Abstraction (fml_arg, _, body) ->
            let body' = subst ~fvars:bvars body fml_arg act_arg' in
            beta_reduce bvars body'
          | _ ->
            app loc fn' act_arg'
  in
  beta_reduce Id.Set.empty

(* Utilities *)

let alpha_equivalent =
  let rec alpha_equiv env tm1 tm2 = match tm1.desc, tm2.desc with
    | Variable id1, Variable id2 ->
      let id1' = try Id.Map.find id1 env with
        | Id.Unbound id ->
          error tm1.loc @@
            Printf.sprintf
              "Term.alpha_equivalent: undefined identifier '%s'"
              (Id.to_string id)
      in
      id1' = id2
    | Abstraction (arg1, tp1, body1), Abstraction (arg2, tp2, body2) ->
      Type.alpha_equivalent tp1 tp2 &&
        alpha_equiv (Id.Map.add arg1 arg2 env) body1 body2
    | Application (fn1, arg1), Application (fn2, arg2) ->
      alpha_equiv env fn1 fn2 && alpha_equiv env arg1 arg2
    | _ ->
      false
  in
  alpha_equiv (Id.Map.empty)

let rec to_string tm =
  let to_paren_string tm = Printf.sprintf "(%s)" (to_string tm) in
  match tm.desc with
    | Variable id ->
      Id.to_string id
    | Abstraction (arg, tp, body) ->
      Printf.sprintf "%s : %s . %s"
        (Id.to_string arg)
        (Type.to_string tp)
        (to_string body)
    | Application (fn, arg) ->
      let fn_to_string tm = match tm.desc with
        | Variable _ -> to_string tm
        | Abstraction _ -> to_paren_string tm
        | Application _ -> to_string tm
      in
      let arg_to_string tm = match tm.desc with
        | Variable _ -> to_string tm
        | Abstraction _ -> to_paren_string tm
        | Application _ -> to_paren_string tm
      in
      Printf.sprintf "%s %s" (fn_to_string fn) (arg_to_string arg)

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
