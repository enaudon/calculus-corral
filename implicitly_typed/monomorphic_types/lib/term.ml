module Id = Identifier
module Loc = Location
module Sub = Type.Substitution
module Type_env = Type.Environment

type desc =
  | Variable of Id.t
  | Abstraction of Id.t * t
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

let var : Loc.t -> Id.t -> t = fun loc id -> { desc = Variable id; loc }

let abs : Loc.t -> Id.t -> t -> t = fun loc arg body ->
  { desc = Abstraction (arg, body); loc }

let app : Loc.t -> t -> t -> t = fun loc fn arg ->
  { desc = Application (fn, arg); loc }

(* Typing *)

(*
  [infer_hm env tm] ensures that [tm] has type [tp], via Algorithm
  W-style Hindley-Milner type inference.  [tm] is assumed to be closed
  under [env].
 *)
let infer_hm : Type_env.t -> Type.t -> t -> Sub.s = fun env exp_tp tm ->

  let unify loc sub tp1 tp2 =
    try Type.unify sub tp1 tp2 with
      | Type.Occurs (id, tp) ->
        error loc "infer_hm" @@
          Printf.sprintf
            "type variable '%s' occurs in '%s'"
            (Id.to_string id)
            (Type.to_string ~no_simp:() tp)
  in

  let rec infer env sub exp_tp tm =
    let loc = tm.loc in
    match tm.desc with
      | Variable id ->
        let tp = try Type_env.find_term id env with
          | Id.Unbound id ->
          error loc "infer_hm" @@
            Printf.sprintf "undefined identifier '%s'" (Id.to_string id)
        in
        unify loc sub exp_tp tp
      | Abstraction (arg, body) ->
        let arg_tp = Type.var @@ Id.gen_upper () in
        let body_tp = Type.var @@ Id.gen_upper () in
        let env' = Type_env.add_term arg arg_tp env in
        let sub' = infer env' sub body_tp body in
        unify loc sub' exp_tp @@ Type.func arg_tp body_tp
      | Application (fn, arg) ->
        let tp = Type.var @@ Id.gen_upper () in
        infer env (infer env sub (Type.func tp exp_tp) fn) tp arg
  in

  infer env Sub.identity exp_tp tm

let to_type_hm env tm =
  let tp = Type.var @@ Id.gen_upper () in
  let sub = infer_hm env tp tm in
  Sub.apply tp sub

(*
  [infer_pr env tp tm] ensures that [tm] has type [tp], via
  constraint-based type inference a la Pottier and Remy.  [tm] is
  assumed to be closed under [env].
 *)
let infer_pr : Type_env.t -> Type.t -> t -> Sub.s = fun env exp_tp tm ->

  let module TC = Type_constraint in

  let rec constrain exp_tp tm =
    let loc = tm.loc in
    match tm.desc with
      | Variable id ->
        TC.var_eq ~loc id exp_tp
      | Abstraction (arg, body) ->
        TC.exists ~loc @@ fun arg_id ->
          let arg_tp = Type.var arg_id in
          TC.exists ~loc @@ fun body_id ->
            let body_tp = Type.var body_id in
            TC.conj
              (TC.def arg arg_tp @@ constrain body_tp body)
              (TC.type_eq exp_tp @@ Type.func arg_tp body_tp)
      | Application (fn, arg) ->
        TC.exists ~loc @@ fun arg_id ->
          let arg_tp = Type.var arg_id in
          TC.conj
            (constrain (Type.func arg_tp exp_tp) fn)
            (constrain arg_tp arg)
  in

  TC.solve @@
    Type_env.fold_term (fun id -> TC.def id) env (constrain exp_tp tm)

let to_type_pr env tm =
  let tp = Type.var @@ Id.gen_upper () in
  let sub = infer_pr env tp tm in
  Sub.apply tp sub

(* Utilities *)

let rec to_string tm =
  let to_paren_string tm = Printf.sprintf "(%s)" (to_string tm) in
  match tm.desc with
    | Variable id ->
      Id.to_string id
    | Abstraction (arg, body) ->
      Printf.sprintf "\\%s . %s"
        (Id.to_string arg)
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

let var ?(loc = Loc.dummy) id = var loc id

let abs ?(loc = Loc.dummy) arg body = abs loc arg body

let abs' ?(loc = Loc.dummy) args body =
  List.fold_right (abs ~loc) args body

let app ?(loc = Loc.dummy) fn arg = app loc fn arg

let app' ?(loc = Loc.dummy) fn args = List.fold_left (app ~loc) fn args
