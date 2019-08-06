module Id = Identifier
module Infer = Type.Inferencer
module IR = Universal_types
module Loc = Location
module Type_env = Type.Environment

type desc =
  | Variable of Id.t
  | Abstraction of Id.t * t
  | Application of t * t
  | Binding of Id.t * t * t

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

let bind : Loc.t -> Id.t -> t -> t -> t = fun loc id value body ->
  { desc = Binding (id, value, body); loc }

(* Typing *)

(* TODO: Comment. *)
let coerce tvs qs ir_tm =

  let module Env = IR.Type.Environment in

  (* Compute unused type variables *)
  let diff = List.fold_left (fun tvs q -> Id.Set.del q tvs) tvs qs in
  let unused = Id.Set.elements diff in

  (* Create a substitution *)
  let id = Id.define "_" in
  let bot = IR.Type.forall id @@ IR.Type.var id in
  let add env id = Env.Type.add id bot env in
  let sub = List.fold_left add Env.empty unused in

  (* Apply the substitution *)
  IR.Term.subst_tp diff sub ir_tm

(*
  [infer_hm env tm] performs two tasks: (a) it infers the type of [tm],
  via Algorithm W-style Hindley-Milner type inference; and (b) it
  constructs an internal representation term which is equivalent to
  [tm].  [tm] is assumed to be closed under [env].
 *)
let infer_hm : Type_env.t -> t -> Type.t * IR.Term.t = fun env tm ->

  let type_to_ir state tp =
    Type.to_intl_repr @@ Infer.apply state tp
  in

  let fresh_inf_var state =
    let tv = Type.inf_var @@ Id.gen_upper () in
    Infer.register state tv, tv
  in

  let unify loc state tp1 tp2 =
    try Infer.unify state tp1 tp2 with
      | Type.Occurs (id, tp) ->
        error loc "infer_hm" @@
          Printf.sprintf
            "type variable '%s' occurs in '%s'"
            (Id.to_string id)
            (Type.to_string ~no_simp:() tp)
  in

  let rec infer env state exp_tp tm =
    let loc = tm.loc in
    match tm.desc with
      | Variable id ->
        let state, tvs, tp =
          try
            Infer.inst state @@ Type_env.Term.find id env
          with Id.Unbound id ->
            error tm.loc "infer_hm" @@
              Printf.sprintf
                "undefined identifier '%s'"
                (Id.to_string id)
        in
        ( unify loc state exp_tp tp,
          fun state ->
            IR.Term.tp_app' ~loc
              (IR.Term.var ~loc id)
              (List.map (type_to_ir state) tvs) )
      | Abstraction (arg, body) ->
        let state, arg_tp = fresh_inf_var state in
        let state, body_tp = fresh_inf_var state in
        let env' = Type_env.Term.add arg arg_tp env in
        let state, body_k = infer env' state body_tp body in
        ( unify loc state exp_tp @@ Type.func arg_tp body_tp,
          fun state ->
            let arg_tp' = type_to_ir state arg_tp in
            IR.Term.abs ~loc arg arg_tp' (body_k state) )
      | Application (fn, arg) ->
        let state, tp = fresh_inf_var state in
        let state, fn_k = infer env state (Type.func tp exp_tp) fn in
        let state, arg_k = infer env state tp arg in
        ( state,
          fun state -> IR.Term.app ~loc (fn_k state) (arg_k state) )
      | Binding (id, value, body) ->
        let state = Infer.gen_enter state in
        let state, mono_tp = fresh_inf_var state in
        let state, value_k = infer env state mono_tp value in
        let state, tvs, poly_tp = Infer.gen_exit state mono_tp in
        let qs = Type.get_quants poly_tp in
        let env' = Type_env.Term.add id poly_tp env in
        let state, body_k = infer env' state exp_tp body in
        ( state,
          fun state ->
            let poly_tp' = type_to_ir state poly_tp in
            IR.Term.app ~loc
              (IR.Term.abs ~loc id poly_tp' (body_k state))
              (coerce tvs qs @@
                IR.Term.tp_abs' ~loc qs @@ value_k state) )
  in

  let state = Infer.gen_enter Infer.initial in
  let state, mono_tp = fresh_inf_var state in
  let state, k = infer env state mono_tp tm in
  let state, tvs, poly_tp = Infer.gen_exit state mono_tp in
  let qs = Type.get_quants poly_tp in
  let tm' =
    coerce tvs qs @@ IR.Term.tp_abs' ~loc:tm.loc qs @@ k state
  in

  poly_tp, tm'

let to_type_hm env tm = fst @@ infer_hm env tm

let to_intl_repr_hm env tm = snd @@ infer_hm env tm

(*
  [infer_pr env tm] performs two tasks: (a) it infers the type of [tm],
  via constraint-based type inference a la Pottier and Remy; and (b) it
  constructs an internal representation term which is equivalent to
  [tm].  [tm] is assumed to be closed under [env].
 *)
let infer_pr : Type_env.t -> t -> Type.t * IR.Term.t = fun env tm ->

  let module TC = Type_constraint in
  let open TC.Operators in

  let rec constrain exp_tp tm =

    let loc = tm.loc in
    match tm.desc with
      | Variable id ->
        TC.inst ~loc id exp_tp <$>
          fun tps ->
            let tps' = List.map Type.to_intl_repr tps in
            IR.Term.tp_app' ~loc (IR.Term.var ~loc id) tps'
      | Abstraction (arg, body) ->
        TC.exists ~loc (fun arg_tp ->
          TC.exists ~loc @@ fun body_tp ->
            TC.conj_left
              (TC.def arg arg_tp @@ constrain body_tp body)
              (TC.equals exp_tp @@ Type.func arg_tp body_tp)) <$>
          fun (arg_tp, (_, body')) ->
            let arg_tp' = Type.to_intl_repr arg_tp in
            IR.Term.abs ~loc arg arg_tp' body'
      | Application (fn, arg) ->
        TC.exists ~loc (fun arg_tp ->
          TC.conj
            (constrain (Type.func arg_tp exp_tp) fn)
            (constrain arg_tp arg)) <$>
          fun (_, (fn', arg')) -> IR.Term.app ~loc fn' arg'
      | Binding (id, value, body) ->
        TC.let_ ~loc id
          (fun tp -> constrain tp value)
          (constrain exp_tp body) <$>
          fun (tp, tvs, value', body') ->
            let tp' = Type.to_intl_repr tp in
            let qs = fst @@ IR.Type.get_forall' tp' in
            IR.Term.app ~loc
              (IR.Term.abs ~loc id tp' body')
              (coerce tvs qs @@ IR.Term.tp_abs' ~loc qs value')

  in

  let loc = tm.loc in
  let c =
    TC.top ~loc
      (fun tp -> constrain tp tm) <$>
      fun (tp, tvs, tm') ->
        let qs = fst @@ IR.Type.get_forall' @@ Type.to_intl_repr tp in
        tp, coerce tvs qs @@ IR.Term.tp_abs' ~loc qs tm'
  in
  TC.solve env c

let to_type_pr env tm = fst @@ infer_pr env tm

let to_intl_repr_pr env tm = snd @@ infer_pr env tm

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

let var ?(loc = Loc.dummy) id = var loc id

let abs ?(loc = Loc.dummy) arg body = abs loc arg body

let abs' ?(loc = Loc.dummy) args body =
  List.fold_right (abs ~loc) args body

let app ?(loc = Loc.dummy) fn arg = app loc fn arg

let app' ?(loc = Loc.dummy) fn args = List.fold_left (app ~loc) fn args

let bind ?(loc = Loc.dummy) id value body = bind loc id value body
