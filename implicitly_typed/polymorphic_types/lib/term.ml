module Id = Identifier
module Loc = Location

type 'a desc =
  | Variable of Id.t
  | Abstraction of 'a * 'a term
  | Application of 'a term * 'a term
  | Binding of 'a * 'a term * 'a term

and 'a term = {
  desc : 'a desc ;
  loc : Loc.t ;
}

type t = Id.t term

(* Internal utilities *)

let error : Loc.t -> string -> 'a = fun loc msg ->
  failwith @@ Printf.sprintf "%s: %s" (Loc.to_string loc) msg

let var loc id = { desc = Variable id; loc }

let abs loc arg body = { desc = Abstraction (arg, body); loc }

let app loc fn arg = { desc = Application (fn, arg); loc }

let bind loc id value body = { desc = Binding (id, value, body); loc }

(* Typing *)

(*
  [default_rank] is the starting rank for expressions.  We use [0] for
  type variables that should never be generalized, and [1] for type
  variables introduced at the top-level.  This leaves rank [2] and
  higher for expression-level type variables.
 *)
let default_rank = 2

(*
  [annotate r tm] constructs an term which is identical to [tm] except
  that abstractions are annotated with types.  [annotate] does not do
  inference, so the types are simply fresh variables with rank [r].
 *)
let rec annotate : int -> t -> (Id.t * Type.t) term = fun rank tm ->
  let loc = tm.loc in
  let annotate = annotate rank in
  match tm.desc with
    | Variable id ->
      var loc id
    | Abstraction (arg, body) ->
      let tp = Type.var rank @@ Id.fresh () in
      abs loc (arg, tp) @@ annotate body
    | Application (fn, arg) ->
      app loc (annotate fn) (annotate arg)
    | Binding (id, value, body) ->
      let tp = Type.var rank @@ Id.fresh () in
      bind loc (id, tp) (annotate value) (annotate body)

(*
  [infer_hm r env tp tm] ensures that [tm] has type [tp], via Algorithm
  W-style Hindley-Milner type inference.  [tm] is assumed to be closed
  under [env].
 *)
let rec infer_hm
    : int -> Type.t Id.Map.t -> Type.t -> (Id.t * Type.t) term -> unit
    = fun rank env exp_tp tm ->

  let unify tp1 tp2 =
    try Type.unify tp1 tp2 with
      | Type.Cannot_unify (tp1, tp2) ->
        error tm.loc @@
          Printf.sprintf
            "Unification failed -- '%s' ~ '%s'"
            (Type.to_string ~no_simp:() tp1)
            (Type.to_string ~no_simp:() tp2)
      | Type.Occurs (id, tp) ->
        error tm.loc @@
          Printf.sprintf
            "Occurs-check failed -- '%s' occurs in '%s'"
            (Id.to_string id)
            (Type.to_string ~no_simp:() tp)
  in

  let infer_hm = infer_hm rank in
  match tm.desc with
    | Variable id ->
      let tp = try Type.inst rank @@ Id.Map.find id env with
        | Id.Unbound id ->
          error tm.loc @@
            Printf.sprintf
              "%s: Undefined identifier '%s'\n%!"
              (Loc.to_string tm.loc)
              (Id.to_string id)
      in
      unify tp exp_tp
    | Abstraction ((arg, arg_tp), body) ->
      let body_tp = Type.var rank @@ Id.fresh () in
      infer_hm (Id.Map.add arg arg_tp env) body_tp body;
      unify exp_tp @@ Type.func arg_tp body_tp
    | Application (fn, arg) ->
      let tp = Type.var rank @@ Id.fresh () in
      infer_hm env (Type.func tp exp_tp) fn;
      infer_hm env tp arg
    | Binding (id_tp, value, body) ->
      infer_hm env exp_tp @@ app tm.loc (abs tm.loc id_tp body) value

let to_type_hm ?(env = Id.Map.empty) tm =
  let tp = Type.var default_rank @@ Id.fresh () in
  infer_hm default_rank env tp @@ annotate default_rank tm;
  tp

(*
  [infer_pr r env tp tm] ensures that [tm] has type [tp], via
  constraint-based type inference a la Pottier and Remy.  [tm] is
  assumed to be closed under [env].
 *)
let infer_pr
    : int -> Type.t Id.Map.t -> Type.t -> (Id.t * Type.t) term -> unit
    = fun rank env exp_tp tm ->

  let module TC = Type_constraint in

  let rec constrain rank exp_tp tm =
    let constrain = constrain rank in
    let loc = tm.loc in
    match tm.desc with
      | Variable id ->
        TC.inst ~loc rank id exp_tp
      | Abstraction ((arg, arg_tp), body) ->
        let body_id = Id.fresh () in
        let body_tp = Type.var rank body_id in
        TC.exists ~loc body_id @@
          TC.conj
            (TC.def arg arg_tp @@ constrain body_tp body)
            (TC.equals exp_tp @@ Type.func arg_tp body_tp)
      | Application (fn, arg) ->
        let arg_id = Id.fresh () in
        let arg_tp = Type.var rank arg_id in
        TC.exists ~loc arg_id @@
          TC.conj
            (constrain (Type.func arg_tp exp_tp) fn)
            (constrain arg_tp arg)
      | Binding (id_tp, value, body) ->
        constrain exp_tp @@ app loc (abs loc id_tp body) value
  in

  TC.solve @@
    Id.Map.fold (fun id -> TC.def id) env (constrain rank exp_tp tm)

let to_type_pr ?(env = Id.Map.empty) tm =
  let tp = Type.var default_rank @@ Id.fresh () in
  infer_pr default_rank env tp @@ annotate default_rank tm;
  tp

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
        | Abstraction _ | Binding _ -> to_paren_string tm
      in
      let arg_to_string tm = match tm.desc with
        | Variable _ ->
          to_string tm
        | Abstraction _ | Application _ | Binding _ ->
          to_paren_string tm
      in
      Printf.sprintf "%s %s" (fn_to_string fn) (arg_to_string arg)
    | Binding (id, value, body) ->
      Printf.sprintf "let %s = %s in %s"
        (Id.to_string id)
        (to_string value)
        (to_string body)

(* Constructors *)

let var ?(loc = Loc.dummy) id = var loc (Id.of_string id)

let abs ?(loc = Loc.dummy) arg body = abs loc (Id.of_string arg) body

let abs' ?(loc = Loc.dummy) args body =
  let abs' body arg = abs ~loc arg body in
  List.fold_left abs' body (List.rev args)

let app ?(loc = Loc.dummy) fn arg = app loc fn arg

let app' ?(loc = Loc.dummy) fn args =
  List.fold_left (fun fn args -> app ~loc fn args) fn args

let bind ?(loc = Loc.dummy) id value body =
  bind loc (Id.of_string id) value body
