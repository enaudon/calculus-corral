module Id = Identifier
module Loc = Location

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

(* [top_rank] is the rank for top-level statements. *)
let top_rank = 1

(* [default_rank] is the starting rank for expressions. *)
let default_rank = 2

(*
  [infer_hm r env tp tm] performs two tasks: (a) it ensures that [tm]
  has type [tp], via Algorithm W-style Hindley-Milner type inference;
  and (b) it constructs an internal representation term which is
  equivalent to [tm].  [tm] is assumed to be closed under [env].
 *)
let infer_hm
    : int -> Type.t Id.Map.t -> Type.t -> t -> Universal_types.Term.t
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

  let rec infer rank env exp_tp tm =
    let module IR = Universal_types.Term in
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
        unify tp exp_tp;
        fun () -> IR.tp_app' ~loc
          (IR.var ~loc @@ Id.to_string id)
          (List.map Type.to_intl_repr tvs)
      | Abstraction (arg, body) ->
        let arg_tp = Type.var rank @@ Id.fresh_upper () in
        let body_tp = Type.var rank @@ Id.fresh_upper () in
        let env' = Id.Map.add arg arg_tp env in
        let body_k = infer rank env' body_tp body in
        unify exp_tp @@ Type.func arg_tp body_tp;
        fun () ->
          let body' = body_k () in
          let arg_tp' = Type.to_intl_repr arg_tp in
          IR.abs ~loc (Id.to_string arg) arg_tp' body'
      | Application (fn, arg) ->
        let tp = Type.var rank @@ Id.fresh_upper () in
        let fn_k = infer rank env (Type.func tp exp_tp) fn in
        let arg_k = infer rank env tp arg in
        fun () -> IR.app ~loc (fn_k ()) (arg_k ())
      | Binding (id, value, body) ->
        let tp = Type.var rank @@ Id.fresh_upper () in
        let value_k = infer (rank + 1) env tp value in
        let tp' = Type.gen rank tp in
        let tvs = Type.get_quants tp' in
        let env' = Id.Map.add id tp' env in
        let body_k = infer rank env' exp_tp body in
        fun () ->
          let value' = value_k () in
          let body' = body_k () in
          IR.app ~loc
            (IR.abs ~loc (Id.to_string id) (Type.to_intl_repr tp') body')
            (IR.tp_abs' ~loc (List.map Id.to_string tvs) value')
  in

  infer rank env exp_tp tm ()

let to_type_hm ?(env = Id.Map.empty) tm =
  let tp = Type.var default_rank @@ Id.fresh_upper () in
  ignore @@ infer_hm default_rank env tp tm;
  Type.gen top_rank tp

let to_intl_repr_hm ?(env = Id.Map.empty) tm =
  let tp = Type.var default_rank @@ Id.fresh_upper () in
  let tm' = infer_hm default_rank env tp tm in
  let tvs = Type.get_quants @@ Type.gen top_rank tp in
  let module IR = Universal_types.Term in
  IR.tp_abs' ~loc:tm.loc (List.map Id.to_string tvs) tm'

(*
  [infer_pr r env tp tm] performs two tasks: (a) it ensures that [tm]
  has type [tp], via constraint-based type inference a la Pottier and
  Remy; and (b) it constructs an internal representation term which is
  equivalent to [tm].  [tm] is assumed to be closed under [env].
 *)
let infer_pr
    : int -> Type.t Id.Map.t -> Type.t -> t -> Universal_types.Term.t
    = fun rank env exp_tp tm ->

  let module TC = Type_constraint in

  let rec constrain rank exp_tp tm =

    let module IRTm = Universal_types.Term in
    let module IRTp = Universal_types.Type in
    let open TC.Operators in

    let loc = tm.loc in
    match tm.desc with
      | Variable id ->
        TC.inst ~loc id exp_tp <$>
          fun tps ->
            IRTm.tp_app' ~loc (IRTm.var ~loc @@ Id.to_string id) tps
      | Abstraction (arg, body) ->
        TC.exists ~loc rank (fun arg_tp ->
          TC.exists ~loc rank @@ fun body_tp ->
            TC.conj
              (TC.def arg arg_tp @@ constrain rank body_tp body)
              (TC.equals exp_tp @@ Type.func arg_tp body_tp)) <$>
          fun (arg_tp, (_, (body', ()))) ->
            IRTm.abs ~loc (Id.to_string arg) arg_tp body'
      | Application (fn, arg) ->
        TC.exists ~loc rank (fun arg_tp ->
          TC.conj
            (constrain rank (Type.func arg_tp exp_tp) fn)
            (constrain rank arg_tp arg)) <$>
          fun (_, (fn', arg')) -> IRTm.app ~loc fn' arg'
      | Binding (id, value, body) ->
        TC.let_ ~loc rank id
          (fun tp -> constrain (rank + 1) tp value)
          (constrain rank exp_tp body) <$>
          fun (tp, value', body') ->
            let tvs = fst @@ IRTp.get_forall' tp in
            IRTm.app ~loc
              (IRTm.abs ~loc (Id.to_string id) tp body')
              (IRTm.tp_abs' ~loc (List.map Id.to_string tvs) value')

  in

  TC.solve rank @@
    Id.Map.fold (fun id -> TC.def id) env (constrain rank exp_tp tm)

let to_type_pr ?(env = Id.Map.empty) tm =
  let tp = Type.var default_rank @@ Id.fresh_upper () in
  ignore @@ infer_pr default_rank env tp tm;
  Type.gen top_rank tp

let to_intl_repr_pr ?(env = Id.Map.empty) tm =
  let tp = Type.var default_rank @@ Id.fresh_upper () in
  let tm' = infer_pr default_rank env tp tm in
  let tvs = Type.get_quants @@ Type.gen top_rank tp in
  let module IR = Universal_types.Term in
  IR.tp_abs' ~loc:tm.loc (List.map Id.to_string tvs) tm'

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
