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
  { desc = Abstraction (arg, tp, body); loc }

let app : Loc.t -> t -> t -> t = fun loc fn arg ->
  { desc = Application (fn, arg); loc }

(* Typing *)

let to_type ?(env = Type.default_env, Id.Map.empty) =
  let rec to_type kn_env tp_env tm = match tm.desc with
    | Variable id ->
      begin try Id.Map.find id tp_env with
        | Id.Unbound id ->
          error tm.loc "to_type" @@
            Printf.sprintf "undefined identifier '%s'" (Id.to_string id)
      end
    | Abstraction (arg, arg_tp, body) ->
      let arg_kn = Type.to_kind ~env:kn_env arg_tp in
      if not (Kind.alpha_equivalent arg_kn Kind.base) then
        error tm.loc "to_type" @@
          Printf.sprintf
            "expected propper type; found '%s'"
            (Type.to_string arg_tp);
      let body_tp = to_type kn_env (Id.Map.add arg arg_tp tp_env) body in
      Type.func arg_tp body_tp
    | Application (fn, arg) ->
      let fn' = to_type kn_env tp_env fn in
      let fml_arg_tp, res_tp =
        try
          Type.get_func (Type.beta_reduce ~deep:() ~env:tp_env fn')
        with Invalid_argument _ ->
          error tm.loc "to_type" @@
            Printf.sprintf
              "expected function type; found '%s'"
              (Type.to_string fn')
      in
      let act_arg_tp = to_type kn_env tp_env arg in
      if
        Type.alpha_equivalent ~beta_env:tp_env act_arg_tp fml_arg_tp
      then
        res_tp
      else
        error arg.loc "to_type" @@
            Printf.sprintf
              "expected type '%s'; found type '%s'"
              (Type.to_string fml_arg_tp)
              (Type.to_string act_arg_tp)
  in
  to_type (fst env) (snd env)

(* Transformations *)

(** [free_vars tm] computes the free variables in [tm]. *)
let free_vars : t -> Id.Set.t =
  let rec free_vars fvs tm = match tm.desc with
    | Variable id -> Id.Set.add id fvs
    | Abstraction (arg, _, body) -> Id.Set.del arg @@ free_vars fvs body
    | Application (fn, arg) -> free_vars (free_vars fvs fn) arg
  in
  free_vars Id.Set.empty

(**
  [subst tm id tm'] replaces occurences of [id] in [tm] with [tm'].

  [subst] avoids name capture by renaming binders in [tm] to follow the
  Barendregt convention--i.e. the names of bound variable are chosen
  distinct from those of free variables.
 *)
let subst : t -> Id.t -> t -> t = fun tm id tm' ->
  let rec subst fvs sub tm =
    let loc = tm.loc in
    match tm.desc with
      | Variable id ->
        Id.Map.find_default tm id sub
      | Abstraction (arg, tp, body) when Id.Set.mem arg fvs ->
        let arg' = Id.fresh () in
        let sub' = Id.Map.add arg (var Loc.dummy arg') sub in
        abs loc arg' tp @@ subst (Id.Set.add arg' fvs) sub' body
      | Abstraction (arg, tp, body) ->
        abs loc arg tp @@
          subst (Id.Set.add arg fvs) (Id.Map.del arg sub) body
      | Application (fn, arg) ->
        app loc (subst fvs sub fn) (subst fvs sub arg)
  in
  subst (free_vars tm') (Id.Map.singleton id tm') tm

let rec beta_reduce ?deep ?(env = Id.Map.empty) tm =
  let beta_reduce = beta_reduce ?deep ~env in
  let loc = tm.loc in
  match tm.desc with
    | Variable id ->
      Id.Map.find_default tm id env
    | Abstraction (arg, tp, body) ->
      if deep <> None then
        abs loc arg tp @@ beta_reduce body
      else
        tm
    | Application (fn, act_arg) ->
      let fn' = beta_reduce fn in
      let act_arg' = beta_reduce act_arg in
      match fn'.desc with
        | Abstraction (fml_arg, _, body) ->
          let body' = subst body fml_arg act_arg' in
          beta_reduce body'
        | _ ->
          app loc fn' act_arg'

(* Utilities *)

let alpha_equivalent =
  let rec alpha_equiv env tm1 tm2 = match tm1.desc, tm2.desc with
    | Variable id1, Variable id2 ->
      Id.alpha_equivalent env id1 id2
    | Abstraction (arg1, tp1, body1), Abstraction (arg2, tp2, body2) ->
      Type.alpha_equivalent tp1 tp2 &&
        alpha_equiv ((arg1, arg2) :: env) body1 body2
    | Application (fn1, arg1), Application (fn2, arg2) ->
      alpha_equiv env fn1 fn2 && alpha_equiv env arg1 arg2
    | _ ->
      false
  in
  alpha_equiv []

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
        | Variable _ | Application _ -> to_string tm
        | Abstraction _ -> to_paren_string tm
      in
      let arg_to_string tm = match tm.desc with
        | Variable _ -> to_string tm
        | Abstraction _ | Application _ -> to_paren_string tm
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