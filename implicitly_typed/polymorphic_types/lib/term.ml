module Id = Identifier
module IR = Universal_types
module Loc = Location
module Sub = Type.Substitution

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

let error : Loc.t -> string -> 'a = fun loc msg ->
  failwith @@ Printf.sprintf "%s: %s" (Loc.to_string loc) msg

let var loc id = { desc = Variable id; loc }

let abs loc arg body = { desc = Abstraction (arg, body); loc }

let app loc fn arg = { desc = Application (fn, arg); loc }

let bind loc id value body = { desc = Binding (id, value, body); loc }

(* Typing *)

(*
  [infer_hm r env tp tm] performs two tasks: (a) it ensures that [tm]
  has type [tp], via Algorithm W-style Hindley-Milner type inference;
  and (b) it constructs an internal representation term which is
  equivalent to [tm].  [tm] is assumed to be closed under [env].
 *)
let infer_hm
    : int -> Type.t Id.Map.t -> Type.t -> t -> Sub.s * IR.Term.t
    = fun rank env exp_tp tm ->

  let unify sub tp1 tp2 =
    try Type.unify sub tp1 tp2 with
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

  let type_to_ir sub tp = Type.to_intl_repr @@ Sub.apply tp sub in

  let rec infer rank env sub exp_tp tm =
    let loc = tm.loc in
    match tm.desc with
      | Variable id ->
        let tvs, tp = try Type.inst rank @@ Id.Map.find id env with
          | Id.Unbound id ->
            error tm.loc @@
              Printf.sprintf
                "%s: Undefined identifier '%s'\n%!"
                (Loc.to_string tm.loc)
                (Id.to_string id)
        in
        ( unify sub tp exp_tp,
          fun sub ->
            IR.Term.tp_app' ~loc
              (IR.Term.var ~loc id)
              (List.map (type_to_ir sub) tvs) )
      | Abstraction (arg, body) ->
        let arg_tp = Type.var rank @@ Id.fresh_upper () in
        let body_tp = Type.var rank @@ Id.fresh_upper () in
        let env' = Id.Map.add arg arg_tp env in
        let sub', body_k = infer rank env' sub body_tp body in
        ( unify sub' exp_tp @@ Type.func arg_tp body_tp,
          fun sub ->
            IR.Term.abs ~loc arg (type_to_ir sub arg_tp) (body_k sub) )
      | Application (fn, arg) ->
        let tp = Type.var rank @@ Id.fresh_upper () in
        let sub', fn_k = infer rank env sub (Type.func tp exp_tp) fn in
        let sub'', arg_k = infer rank env sub' tp arg in
        ( sub'', fun sub -> IR.Term.app ~loc (fn_k sub) (arg_k sub) )
      | Binding (id, value, body) ->
        let tp = Type.var (rank + 1) @@ Id.fresh_upper () in
        let sub', value_k = infer (rank + 1) env sub tp value in
        let tp' = Type.gen rank tp in
        let tvs = Type.get_quants tp' in
        let env' = Id.Map.add id tp' env in
        let sub'', body_k = infer rank env' sub' exp_tp body in
        ( sub'',
          fun sub ->
            IR.Term.app ~loc
              (IR.Term.abs ~loc id (type_to_ir sub tp') (body_k sub))
              (IR.Term.tp_abs' ~loc tvs @@ value_k sub) )
  in

  let sub, k = infer rank env Sub.identity exp_tp tm in
  sub, k sub

let to_type_hm env tm =
  let tp = Type.var (Type.top_rank + 1) @@ Id.fresh_upper () in
  let sub, _ = infer_hm (Type.top_rank + 1) env tp tm in
  Type.gen Type.top_rank @@ Sub.apply tp sub

let to_intl_repr_hm env tm =
  let tp = Type.var (Type.top_rank + 1) @@ Id.fresh_upper () in
  let sub, tm' = infer_hm (Type.top_rank + 1) env tp tm in
  let tp' = Type.gen Type.top_rank @@ Sub.apply tp sub in
  IR.Term.tp_abs' ~loc:tm.loc (Type.get_quants tp') tm'

(*
  [infer_pr r env tp tm] performs two tasks: (a) it ensures that [tm]
  has type [tp], via constraint-based type inference a la Pottier and
  Remy; and (b) it constructs an internal representation term which is
  equivalent to [tm].  [tm] is assumed to be closed under [env].
 *)
let infer_pr
    : int -> Type.t Id.Map.t -> Type.t -> t -> Sub.s * IR.Term.t
    = fun rank env exp_tp tm ->

  let module TC = Type_constraint in

  let rec constrain exp_tp tm =

    let open TC.Operators in

    let loc = tm.loc in
    match tm.desc with
      | Variable id ->
        TC.inst ~loc id exp_tp <$>
          fun tps -> IR.Term.tp_app' ~loc (IR.Term.var ~loc id) tps
      | Abstraction (arg, body) ->
        TC.exists ~loc (fun arg_tp ->
          TC.exists' ~loc @@ fun body_tp ->
            TC.conj_left
              (TC.def arg arg_tp @@ constrain body_tp body)
              (TC.equals exp_tp @@ Type.func arg_tp body_tp)) <$>
          fun (arg_tp, body') -> IR.Term.abs ~loc arg arg_tp body'
      | Application (fn, arg) ->
        TC.exists' ~loc (fun arg_tp ->
          TC.conj
            (constrain (Type.func arg_tp exp_tp) fn)
            (constrain arg_tp arg)) <$>
          fun (fn', arg') -> IR.Term.app ~loc fn' arg'
      | Binding (id, value, body) ->
        TC.let_ ~loc id
          (fun tp -> constrain tp value)
          (constrain exp_tp body) <$>
          fun (tp, value', body') ->
            let tvs = fst @@ IR.Type.get_forall' tp in
            IR.Term.app ~loc
              (IR.Term.abs ~loc id tp body')
              (IR.Term.tp_abs' ~loc tvs value')

  in

  TC.solve rank @@
    Id.Map.fold (fun id -> TC.def id) env (constrain exp_tp tm)

let to_type_pr env tm =
  let tp = Type.var (Type.top_rank + 1) @@ Id.fresh_upper () in
  let sub, _ = infer_pr (Type.top_rank + 1) env tp tm in
  Type.gen Type.top_rank @@ Sub.apply tp sub

let to_intl_repr_pr env tm =
  let tp = Type.var (Type.top_rank + 1) @@ Id.fresh_upper () in
  let sub, tm' = infer_pr (Type.top_rank + 1) env tp tm in
  let tp' = Type.gen Type.top_rank @@ Sub.apply tp sub in
  IR.Term.tp_abs' ~loc:tm.loc (Type.get_quants tp') tm'

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
