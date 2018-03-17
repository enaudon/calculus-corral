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

(* Typing *)

let to_type_hm =
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
      begin try Type.unify fn_tp (Type.func arg_tp res_tp) with
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
  to_type Id.Map.empty

let rec constrain : Type.t -> t -> Type_constraint.t = fun exp_tp tm ->
  let module TC = Type_constraint in
  let loc = tm.loc in
  match tm.desc with
    | Variable id ->
      TC.var_eq ~loc id exp_tp
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

let to_type_pr tm =
  let tp = Type.var @@ Id.fresh () in
  Type_constraint.solve @@ constrain tp tm;
  tp

(* Utilities *)

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

let var ?(loc = Loc.dummy) id =
  { desc = Variable (Id.of_string id); loc }

let abs ?(loc = Loc.dummy) arg body =
  { desc = Abstraction (Id.of_string arg, body); loc }

let abs' ?(loc = Loc.dummy) args body =
  let abs' body arg = abs ~loc arg body in
  List.fold_left abs' body (List.rev args)

let app ?(loc = Loc.dummy) fn arg =
  { desc = Application (fn, arg); loc }

let app' ?(loc = Loc.dummy) fn args =
  List.fold_left (fun fn args -> app ~loc fn args) fn args
