module Id = Identifier
module IR = Records_and_variants
module Loc = Location
module Misc = Miscellaneous
module State = Type.State

type desc =
  | Variable of Id.t
  | Abstraction of Id.t * t
  | Application of t * t
  | Binding of Id.t * t * t
  | Record of (Id.t * t) list
  | Projection of t * Id.t
  | Variant of Id.t * t
  | Case of t * (Id.t * Id.t * t) list

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

let var loc id = { desc = Variable id; loc }

let abs loc arg body = { desc = Abstraction (arg, body); loc }

let app loc fn arg = { desc = Application (fn, arg); loc }

let bind loc id value body = { desc = Binding (id, value, body); loc }

let rcrd : Loc.t -> (Id.t * t) list -> t = fun loc fields ->
  { desc = Record fields; loc }

let proj : Loc.t -> t -> Id.t -> t = fun loc rcrd field ->
  { desc = Projection (rcrd, field); loc }

let vrnt : Loc.t -> Id.t -> t -> t = fun loc case data ->
  { desc = Variant (case, data); loc }

let case : Loc.t -> t -> (Id.t * Id.t * t) list -> t =
    fun loc vrnt cases ->
  { desc = Case (vrnt, cases); loc }

(* Typing *)

(* TODO: Comment. *)
let coerce tvs qs ir_tm =

  (* Compute unused type variables *)
  let diff_fn tvs (q, _) = Id.Map.del q tvs in
  let unused = List.fold_left diff_fn tvs qs in

  (* Create a substitution *)
  let id = Id.of_string "_" in
  let bot kn = IR.Type.forall id kn @@ IR.Type.var id in
  let sub = Id.Map.map (fun kn -> bot @@ Kind.to_intl_repr kn) unused in

  (* Apply the substitution *)
  let fvs = Id.Set.of_list @@ Id.Map.keys unused in
  IR.Term.subst_tp fvs sub ir_tm

(*
  [infer_hm r env tp tm] performs two tasks: (a) it infers the type of
  [tm], via Algorithm W-style Hindley-Milner type inference; and (b) it
  constructs an internal representation term which is equivalent to
  [tm].  [tm] is assumed to be closed under [env].
 *)
let infer_hm
    : Type.t Id.Map.t -> t -> Type.t * IR.Term.t
    = fun env tm ->

  let type_to_ir state tp =
    Type.to_intl_repr @@ State.apply_solution tp state
  in

  let quant_to_ir (q, kn) = q, Kind.to_intl_repr kn in

  let fresh_type_var state kn =
    let tv = Type.var @@ Id.fresh_upper () in
    Type.register state tv kn, tv
  in

  let fresh_type_var_list state n kn =
    let init_fn _ = Type.var @@ Id.fresh_upper () in
    let tvs = Misc.list_init n init_fn in
    let fold_fn tv state = Type.register state tv kn in
    let state = List.fold_right fold_fn tvs state in
    state, tvs
  in

  let unify loc state tp1 tp2 =
    try Type.unify state tp1 tp2 with
      | Type.Occurs (id, tp) ->
        error loc "infer_hm" @@
          Printf.sprintf
            "type variable '%s' occurs in '%s'"
            (Id.to_string id)
            (Type.to_string ~no_simp:() tp)
      | Type.Cannot_unify (tp1, tp2) ->
        error loc "hm_infer" @@
          Printf.sprintf
            "cannot unify '%s' and '%s'"
            (Type.to_string ~no_simp:() tp1)
            (Type.to_string ~no_simp:() tp2)
  in

  let rec infer env state exp_tp tm =
    let loc = tm.loc in
    match tm.desc with
      | Variable id ->
        let state, tvs, tp =
          try
            Type.inst state @@ Id.Map.find id env
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
        let state, arg_tp = fresh_type_var state Kind.prop in
        let state, body_tp = fresh_type_var state Kind.prop in
        let env' = Id.Map.add arg arg_tp env in
        let state, body_k = infer env' state body_tp body in
        ( unify loc state exp_tp @@ Type.func arg_tp body_tp,
          fun state ->
            let arg_tp' = type_to_ir state arg_tp in
            IR.Term.abs ~loc arg arg_tp' (body_k state) )
      | Application (fn, arg) ->
        let state, tp = fresh_type_var state Kind.prop in
        let state, fn_k = infer env state (Type.func tp exp_tp) fn in
        let state, arg_k = infer env state tp arg in
        ( state,
          fun state -> IR.Term.app ~loc (fn_k state) (arg_k state) )
      | Binding (id, value, body) ->
        let state = Type.gen_enter state in
        let state, tp = fresh_type_var state Kind.prop in
        let state, value_k = infer env state tp value in
        let state, tvs, tp' = Type.gen_exit state tp in
        let qs = Type.get_quants tp' in
        let env' = Id.Map.add id tp' env in
        let state, body_k = infer env' state exp_tp body in
        ( state,
          fun state ->
            let tp'' = type_to_ir state tp' in
            let qs' = List.map quant_to_ir qs in
            IR.Term.app ~loc
              (IR.Term.abs ~loc id tp'' (body_k state))
              (coerce tvs qs' @@
                IR.Term.tp_abs' ~loc qs' @@ value_k state) )
      | Record fields ->
        let infer_field (state, ks) tp (id, tm) =
          let state, k = infer env state tp tm in
          state, (id, k) :: ks
        in
        let state, tps =
          fresh_type_var_list state (List.length fields) Kind.prop
        in
        let state, field_ks =
          let s, ks =
            List.fold_left2 infer_field (state, []) tps fields
          in
          s, List.rev ks
        in
        let state =
          unify loc state exp_tp @@
            Type.rcrd (List.combine (List.map fst fields) tps) None
        in
        ( state,
          fun state ->
            IR.Term.rcrd ~loc @@
              List.map (fun (id, k) -> id, k state) field_ks )
      | Projection (rcrd, field) ->
        let state, rest_tp = fresh_type_var state Kind.row in
        let tp = Type.rcrd [(field, exp_tp)] @@ Some rest_tp in
        let state, k = infer env state tp rcrd in
        ( state, fun state -> IR.Term.proj ~loc (k state) field)
      | Variant (case, data) ->
        let state, data_tp = fresh_type_var state Kind.prop in
        let state, rest_tp = fresh_type_var state Kind.row in
        let state, data_k = infer env state data_tp data in
        let tp = Type.vrnt [(case, data_tp)] @@ Some rest_tp in
        let state = unify loc state exp_tp tp in
        ( state,
          fun state ->
            let data_tp' = type_to_ir state data_tp in
            let rest_tp' = type_to_ir state rest_tp in
            IR.Term.vrnt ~loc case (data_k state) @@
              IR.Type.vrnt [(case, data_tp')] @@ Some rest_tp' )
      | Case (vrnt, cases) ->
        let infer_case res_tp (state, ks) tp (case, id, body) =
          let state, k =
            infer (Id.Map.add id tp env) state res_tp body
          in
          state, (case, id, k) :: ks
        in
        let state, tps =
          fresh_type_var_list state (List.length cases) Kind.prop
        in
        let case_tps =
          List.combine (List.map Misc.fst_of_3 cases) tps
        in
        let state, res_tp = fresh_type_var state Kind.prop in
        let state, vrnt_k =
          infer env state (Type.vrnt case_tps None) vrnt
        in
        let state, case_ks =
          let s, ks =
            List.fold_left2 (infer_case res_tp) (state, []) tps cases
          in
          s, List.rev ks
        in
        ( unify loc state exp_tp res_tp,
          fun state ->
            let cases' =
              List.map (fun (case, id, k) -> case, id, k state) case_ks
            in
            IR.Term.case ~loc (vrnt_k state) cases' )
  in

  let state = Type.gen_enter State.initial in
  let state, tp = fresh_type_var state Kind.prop in
  let state, k = infer env state tp tm in
  let state, tvs, tp' = Type.gen_exit state tp in
  let qs = List.map quant_to_ir @@ Type.get_quants tp' in
  let tm' =
    coerce tvs qs @@ IR.Term.tp_abs' ~loc:tm.loc qs @@ k state
  in

  tp', tm'

let to_type_hm env tm = fst @@ infer_hm env tm

let to_intl_repr_hm env tm = snd @@ infer_hm env tm

(*
  [infer_pr env tm] performs two tasks: (a) it infers the type of [tm],
  via constraint-based type inference a la Pottier and Remy; and (b) it
  constructs an internal representation term which is equivalent to
  [tm].  [tm] is assumed to be closed under [env].
 *)
let infer_pr
    : Type.t Id.Map.t -> t -> Type.t * IR.Term.t
    = fun env tm ->

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
        TC.exists ~loc Kind.prop (fun arg_tp ->
          TC.exists ~loc Kind.prop @@ fun body_tp ->
            TC.conj_left
              (TC.def arg arg_tp @@ constrain body_tp body)
              (TC.equals exp_tp @@ Type.func arg_tp body_tp)) <$>
          fun (arg_tp, (_, body')) ->
            let arg_tp' = Type.to_intl_repr arg_tp in
            IR.Term.abs ~loc arg arg_tp' body'
      | Application (fn, arg) ->
        TC.exists ~loc Kind.prop (fun arg_tp ->
          TC.conj
            (constrain (Type.func arg_tp exp_tp) fn)
            (constrain arg_tp arg)) <$>
          fun (_, (fn', arg')) -> IR.Term.app ~loc fn' arg'
      | Binding (id, value, body) ->
        TC.let_ ~loc id Kind.prop
          (fun tp -> constrain tp value)
          (constrain exp_tp body) <$>
          fun (tp, tvs, value', body') ->
            let tp' = Type.to_intl_repr tp in
            let qs = fst @@ IR.Type.get_forall' tp' in
            IR.Term.app ~loc
              (IR.Term.abs ~loc id tp' body')
              (coerce tvs qs @@ IR.Term.tp_abs' ~loc qs value')
      | Record fields ->
        let constrain_field tp (id, tm) =
          constrain tp tm <$> fun tm' -> id, tm'
        in
        let kns =
          Misc.list_init (List.length fields) (fun _ -> Kind.prop)
        in
        TC.exists_list ~loc kns (fun tps ->
          let field_tps = List.combine (List.map fst fields) tps in
          TC.conj_left
            (TC.conj_list (List.map2 constrain_field tps fields))
            (TC.equals exp_tp @@ Type.rcrd field_tps None)) <$>
          fun (_, fields') -> IR.Term.rcrd ~loc fields'
      | Projection (rcrd, field) ->
        TC.exists ~loc Kind.row (fun rest_tp ->
          let rcrd_tp = Type.rcrd [(field, exp_tp)] @@ Some rest_tp in
          constrain rcrd_tp rcrd) <$>
        fun (_, rcrd') -> IR.Term.proj ~loc rcrd' field
      | Variant (case, data) ->
        TC.exists ~loc Kind.prop (fun data_tp ->
          TC.exists ~loc Kind.row @@ fun rest_tp ->
            let tp = Type.vrnt [(case, data_tp)] @@ Some rest_tp in
            TC.conj_left
              (constrain data_tp data)
              (TC.equals exp_tp tp)) <$>
          fun (data_tp, (rest_tp, data')) ->
            let data_tp' = Type.to_intl_repr data_tp in
            let rest_tp' = Type.to_intl_repr rest_tp in
            IR.Term.vrnt ~loc case data' @@
              IR.Type.vrnt [(case, data_tp')] @@ Some rest_tp'
      | Case (vrnt, cases) ->
        let constrain_case res_tp tp (case, id, body) =
          TC.def id tp @@ constrain res_tp body <$>
            fun body' -> case, id, body'
        in
        let kns =
          Misc.list_init (List.length cases) (fun _ -> Kind.prop)
        in
        TC.exists_list ~loc kns (fun tps ->
          let case_tps =
            List.combine (List.map Misc.fst_of_3 cases) tps
          in
          TC.exists ~loc Kind.prop @@ fun res_tp ->
            TC.conj_left
              (TC.conj
                (constrain (Type.vrnt case_tps None) vrnt)
                (TC.conj_list
                  (List.map2 (constrain_case res_tp) tps cases)))
              (TC.equals exp_tp res_tp)) <$>
          fun (_, (_, (vrnt', cases'))) ->
            IR.Term.case ~loc vrnt' cases'
  in

  let loc = tm.loc in
  let c =
    TC.top ~loc
      Kind.prop
      (fun tp -> constrain tp tm) <$>
      fun (tp, tvs, tm') ->
        let qs = fst @@ IR.Type.get_forall' @@ Type.to_intl_repr tp in
        tp, coerce tvs qs @@ IR.Term.tp_abs' ~loc qs tm'
  in
  let c = Id.Map.fold (fun id -> TC.def id) env c in
  try
    TC.solve c
  with
    | Failure msg ->
      Printf.printf
        ">> Unable to solve:\n  %s\n%!"
        (TC.to_string ~no_simp:() c);
      failwith msg

let to_type_pr env tm = fst @@ infer_pr env tm

let to_intl_repr_pr env tm = snd @@ infer_pr env tm

(* Utilities *)

let rec to_string tm =

  let to_paren_string tm = Printf.sprintf "(%s)" (to_string tm) in

  let arg_to_string tm = match tm.desc with
    | Variable _ | Record _ | Projection _ ->
      to_string tm
    | Abstraction _ | Application _ | Binding _ | Variant _ | Case _ ->
      to_paren_string tm
  in

  match tm.desc with
    | Variable id ->
      Id.to_string id
    | Abstraction (arg, body) ->
      Printf.sprintf "\\%s . %s"
        (Id.to_string arg)
        (to_string body)
    | Application (fn, arg) ->
      let fn_to_string tm = match tm.desc with
        | Variable _ | Application _ | Record _ | Projection _ ->
          to_string tm
        | Abstraction _ | Binding _ | Variant _ | Case _ ->
          to_paren_string tm
      in
      Printf.sprintf "%s %s" (fn_to_string fn) (arg_to_string arg)
    | Binding (id, value, body) ->
      Printf.sprintf "let %s = %s in %s"
        (Id.to_string id)
        (to_string value)
        (to_string body)
    | Record fields ->
      let field_to_string (id, tm) =
        Printf.sprintf "%s = %s"
          (Id.to_string id)
          (to_string tm)
      in
      Printf.sprintf "{%s}"
        (String.concat "; " @@ List.map field_to_string fields)
    | Projection (rcrd, field) ->
      Printf.sprintf "%s.%s" (arg_to_string rcrd) (Id.to_string field)
    | Variant (case, data) ->
      Printf.sprintf "%s %s" (Id.to_string case) (arg_to_string data)
    | Case (vrnt, cases) ->
      let case_to_string (case, id, tm) =
        Printf.sprintf "%s %s -> %s"
          (Id.to_string case)
          (Id.to_string id)
          (to_string tm)
      in
      Printf.sprintf "case %s [%s]"
        (to_string vrnt)
        (String.concat "; " @@ List.map case_to_string cases)

(* Constructors *)

let var ?(loc = Loc.dummy) id = var loc (Id.of_string id)

let abs ?(loc = Loc.dummy) arg body = abs loc (Id.of_string arg) body

let abs' ?(loc = Loc.dummy) args body =
  List.fold_right (abs ~loc) args body

let app ?(loc = Loc.dummy) fn arg = app loc fn arg

let app' ?(loc = Loc.dummy) fn args = List.fold_left (app ~loc) fn args

let bind ?(loc = Loc.dummy) id value body =
  bind loc (Id.of_string id) value body

let rcrd ?(loc = Loc.dummy) fields =
  rcrd loc @@ List.map (fun (id, tm) -> Id.of_string id, tm) fields

let proj ?(loc = Loc.dummy) rcrd field =
  proj loc rcrd @@ Id.of_string field

let vrnt ?(loc = Loc.dummy) case data =
  vrnt loc (Id.of_string case) data

let case ?(loc = Loc.dummy) vrnt cases =
  let fn (case, id, tm) = Id.of_string case, Id.of_string id, tm in
  case loc vrnt @@ List.map fn cases
