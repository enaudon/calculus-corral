module Id = Identifier
module Loc = Location

type 'a desc =
  | Variable of Id.t
  | Abstraction of 'a * 'a term
  | Application of 'a term * 'a term

and 'a term = {
  desc : 'a desc ;
  loc : Loc.t ;
}

type t = Id.t term

(* Internal utilities *)

let var loc id = { desc = Variable id; loc }

let abs loc arg body = { desc = Abstraction (arg, body); loc }

let app loc fn arg = { desc = Application (fn, arg); loc }

(* Typing *)

(*
  [infer_hm env tm] computes the types of the variables in [tm] (which
  is assumed to be closed under [env]), via Algorithm W-style
  Hindley-Milner type inference.  It constructs an annotated term [tm']
  which is identical to [tm] except that variables are annotated at
  introduction, and it [infer] computes the type of [tm].
 *)
let infer_hm
    : Type.t Id.Map.t -> t -> Type.t * (Id.t * Type.t) term
    = fun env tm ->

  let rec infer env exp_tp tm =
    let loc = tm.loc in
    match tm.desc with
      | Variable id ->
        let tp = try Id.Map.find id env with
          | Id.Unbound id ->
            failwith @@
              Printf.sprintf
                "%s: Undefined identifier '%s'\n%!"
                (Loc.to_string tm.loc)
                (Id.to_string id)
        in
        Type.unify tp exp_tp;
        var loc id
      | Abstraction (arg, body) ->
        let arg_tp = Type.var @@ Id.fresh () in
        let body_tp = Type.var @@ Id.fresh () in
        let body' = infer (Id.Map.add arg arg_tp env) body_tp body in
        Type.unify exp_tp @@ Type.func arg_tp body_tp;
        abs loc (arg, arg_tp) body'
      | Application (fn, arg) ->
        let tp = Type.var @@ Id.fresh () in
        let fn' = infer env (Type.func tp exp_tp) fn in
        let arg' = infer env tp arg in
        app loc fn' arg'
  in

  let tp = Type.var @@ Id.fresh () in
  let tm' = infer env tp tm in
  tp, tm'

let to_type_hm ?(env = Id.Map.empty) tm = fst @@ infer_hm env tm

(*
  [infer_pr env tm] computes the types of the variables in [tm] (which
  is assumed to be closed under [env]), via constraint-based type
  inference a la Pottier and Remy.  It constructs an annotated term
  [tm'] which is identical to [tm] except that variables are annotated
  at introduction, and it [infer] computes the type of [tm].
 *)
let infer_pr
    : Type.t Id.Map.t -> t -> Type.t * (Id.t * Type.t) term
    = fun env tm ->

  let module TC = Type_constraint in

  (*
    TODO: Find a better way to handle returning constraints and terms.
   *)
  let rec constrain env exp_tp tm =
    let constrain = constrain env in
    let loc = tm.loc in
    match tm.desc with
      | Variable id ->
        let tc =
          try
            TC.type_eq ~loc (Id.Map.find id env) exp_tp
          with Id.Unbound _ ->
            TC.var_eq ~loc id exp_tp
        in
        tc, var loc id
      | Abstraction (arg, body) ->
        let arg_id, body_id = Id.fresh (), Id.fresh () in
        let arg_tp, body_tp = Type.var arg_id, Type.var body_id in
        let body_tc, body' = constrain body_tp body in
        let tc =
          TC.exists' ~loc [ arg_id; body_id ] @@
            TC.conj
              (TC.def arg arg_tp body_tc)
              (TC.type_eq exp_tp @@ Type.func arg_tp body_tp)
        in
        tc, abs loc (arg, arg_tp) body'
      | Application (fn, arg) ->
        let arg_id = Id.fresh () in
        let arg_tp = Type.var arg_id in
        let fn_tc, fn' = constrain (Type.func arg_tp exp_tp) fn in
        let arg_tc, arg' = constrain arg_tp arg in
        let tc = TC.exists ~loc arg_id @@ TC.conj fn_tc arg_tc in
        tc, app loc fn' arg'
  in
  let tp = Type.var @@ Id.fresh () in
  let tc, tm' = constrain env tp tm in
  TC.solve tc;
  tp, tm'

let to_type_pr ?(env = Id.Map.empty) tm = fst @@ infer_pr env tm

(* Utilities *)

(** [free_vars tm] computes the free variables in [tm]. *)
let free_vars : t -> Id.Set.t =
  let rec free_vars fvs tm = match tm.desc with
    | Variable id -> Id.Set.add id fvs
    | Abstraction (arg, body) -> Id.Set.del arg @@ free_vars fvs body
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
      | Abstraction (arg, body) when Id.Set.mem arg fvs ->
        let arg' = Id.fresh () in
        let sub' = Id.Map.add arg (var Loc.dummy arg') sub in
        abs loc arg' @@ subst (Id.Set.add arg' fvs) sub' body
      | Abstraction (arg, body) ->
        abs loc arg @@
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
    | Abstraction (arg, body) ->
      if deep <> None then
        abs loc arg @@ beta_reduce body
      else
        tm
    | Application (fn, act_arg) ->
      let fn' = beta_reduce fn in
      let act_arg' = beta_reduce act_arg in
      match fn'.desc with
        | Abstraction (fml_arg, body) ->
          let body' = subst body fml_arg act_arg' in
          beta_reduce body'
        | _ ->
          app loc fn' act_arg'

let rec to_string tm =
  let to_paren_string tm = Printf.sprintf "(%s)" (to_string tm) in
  match tm.desc with
    | Variable id ->
      Id.to_string id
    | Abstraction (arg, body) ->
      Printf.sprintf "%s . %s"
        (Id.to_string arg)
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

let abs ?(loc = Loc.dummy) arg body = abs loc (Id.of_string arg) body

let abs' ?(loc = Loc.dummy) args body =
  let abs' body arg = abs ~loc arg body in
  List.fold_left abs' body (List.rev args)

let app ?(loc = Loc.dummy) fn arg = app loc fn arg

let app' ?(loc = Loc.dummy) fn args =
  List.fold_left (fun fn args -> app ~loc fn args) fn args
