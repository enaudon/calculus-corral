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
  [annotate tm] constructs an term which is identical to [tm] except
  that abstractions are annotated with types.  [annotate] does not do
  inference, so the types are simply fresh variables.
 *)
let rec annotate : t -> (Id.t * Type.t) term = fun tm ->
  let loc = tm.loc in
  match tm.desc with
    | Variable id ->
      var loc id
    | Abstraction (arg, body) ->
      let tp = Type.var @@ Id.fresh () in
      abs loc (arg, tp) @@ annotate body
    | Application (fn, arg) ->
      app loc (annotate fn) (annotate arg)

(*
  [infer_hm env tp tm] ensures that [tm] has type [tp], via Algorithm
  W-style Hindley-Milner type inference.  [tm] is assumed to be closed
  under [env].
 *)
let rec infer_hm
    : Type.t Id.Map.t -> Type.t -> (Id.t * Type.t) term -> unit
    = fun env exp_tp tm ->
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
      Type.unify tp exp_tp
    | Abstraction ((arg, arg_tp), body) ->
      let body_tp = Type.var @@ Id.fresh () in
      infer_hm (Id.Map.add arg arg_tp env) body_tp body;
      Type.unify exp_tp @@ Type.func arg_tp body_tp
    | Application (fn, arg) ->
      let tp = Type.var @@ Id.fresh () in
      infer_hm env (Type.func tp exp_tp) fn;
      infer_hm env tp arg

let to_type_hm ?(env = Id.Map.empty) tm =
  let tp = Type.var @@ Id.fresh () in
  infer_hm env tp @@ annotate tm;
  tp

(*
  [infer_pr env tp tm] ensures that [tm] has type [tp], via
  constraint-based type inference a la Pottier and Remy.  [tm] is
  assumed to be closed under [env].
 *)
let infer_pr
    : Type.t Id.Map.t -> Type.t -> (Id.t * Type.t) term -> unit
    = fun env exp_tp tm ->

  let module TC = Type_constraint in

  let rec constrain env exp_tp tm =
    let constrain = constrain env in
    let loc = tm.loc in
    match tm.desc with
      | Variable id ->
        begin try
          TC.type_eq ~loc (Id.Map.find id env) exp_tp
        with Id.Unbound _ ->
          TC.var_eq ~loc id exp_tp
        end
      | Abstraction ((arg, arg_tp), body) ->
        let body_id = Id.fresh () in
        let body_tp = Type.var body_id in
        TC.exists ~loc body_id @@
          TC.conj
            (TC.def arg arg_tp @@ constrain body_tp body)
            (TC.type_eq exp_tp @@ Type.func arg_tp body_tp)
      | Application (fn, arg) ->
        let arg_id = Id.fresh () in
        let arg_tp = Type.var arg_id in
        TC.exists ~loc arg_id @@
          TC.conj
            (constrain (Type.func arg_tp exp_tp) fn)
            (constrain arg_tp arg)
  in

  TC.solve @@ constrain env exp_tp tm

let to_type_pr ?(env = Id.Map.empty) tm =
  let tp = Type.var @@ Id.fresh () in
  infer_pr env tp @@ annotate tm;
  tp

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
