module Id = Identifier
module Loc = Location

type desc =
  | Variable of Id.t
  | Abstraction of Id.t * t
  | Application of t * t

and t = {
  desc : desc ;
  loc : Loc.t ;
}

(* Internal utilities *)

let var loc id = { desc = Variable id; loc }

let abs loc arg body = { desc = Abstraction (arg, body); loc }

let app loc fn arg = { desc = Application (fn, arg); loc }

(* Typing *)

let to_type_hm ?(env = Id.Map.empty, Id.Map.empty) =

  let rec to_type env tm = match tm.desc with
    | Variable id ->
      begin try Id.Map.find id env with
        | Id.Unbound id ->
          failwith @@
            Printf.sprintf
              "%s: Undefined identifier '%s'\n%!"
              (Loc.to_string tm.loc)
              (Id.to_string id)
      end
    | Abstraction (arg, body) ->
      let arg_tp = Type.var @@ Id.fresh () in
      let body_tp = to_type (Id.Map.add arg arg_tp env) body in
      Type.func arg_tp body_tp
    | Application (fn, arg) ->
      let res_tp = Type.var @@ Id.fresh () in
      let fn_tp = to_type env fn in
      let arg_tp = to_type env arg in
      begin try Type.unify fn_tp @@ Type.func arg_tp res_tp with
        | Type.Occurs (id, tp) ->
          failwith @@
            Printf.sprintf
              "%s: Occurs check failed -- '%s' occurs in '%s'"
              (Loc.to_string tm.loc)
              (Id.to_string id)
              (Type.to_string tp)
      end;
      res_tp
  in

  to_type (snd env)

let to_type_pr ?(env = Id.Map.empty, Id.Map.empty) tm =

  let module TC = Type_constraint in

  let rec constrain env exp_tp tm =
    let constrain = constrain env in
    let loc = tm.loc in
    match tm.desc with
      | Variable id ->
        begin try
          TC.type_eq ~loc exp_tp @@ Id.Map.find id env
        with Id.Unbound _ ->
          TC.var_eq ~loc id exp_tp
        end
      | Abstraction (arg, body) ->
        TC.exists ~loc @@ fun id1 ->
          let arg_tp = Type.var id1 in
          TC.exists (fun id2 ->
            let body_tp = Type.var id2 in
            TC.conj
              (TC.def arg arg_tp @@ constrain body_tp body)
              (TC.type_eq exp_tp @@ Type.func arg_tp body_tp))
      | Application (fn, arg) ->
        TC.exists ~loc @@ fun id ->
          let arg_tp = Type.var id in
          TC.conj
            (constrain (Type.func arg_tp exp_tp) fn)
            (constrain arg_tp arg)
  in

  let tp = Type.var @@ Id.fresh () in
  TC.solve @@ constrain (snd env) tp tm;
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
