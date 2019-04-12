module Id = Identifier
module Kind_env = Kind.Environment
module Misc = Miscellaneous

type morph = Mono | Poly

(* The type of monomorphic types. *)
type mono =
  | Inference_variable of morph * Id.t
  | Variable of Id.t
  | Application of mono * mono
  | Row_nil
  | Row_cons of Id.t * mono * mono

(* The type of types schemes. *)
type t = {
  quants : (Id.t * Kind.t) list ;
  body : mono ;
}

module Environment = Type_environment.Make (struct
  type value = t
  let initial_types = []
  let initial_terms = []
end)

(* Exceptions *)

exception Occurs of Id.t * t

exception Cannot_unify of t * t

(* Internal functions *)

let error : string -> string -> 'a = fun fn_name msg ->
  failwith @@ Printf.sprintf "%s.%s: %s" __MODULE__ fn_name msg

let expected_mono_internal : string -> string -> 'a =
    fun modl_name fn_name ->
  invalid_arg @@
    Printf.sprintf "%s.%s: expected monomorphic type" modl_name fn_name

let expected_mono = expected_mono_internal __MODULE__

let inf_var : morph -> Id.t -> mono = fun morph id ->
  Inference_variable (morph, id)

let var : Id.t -> mono = fun id -> Variable id

let app : mono -> mono -> mono = fun fn arg -> Application (fn, arg)

let row_nil : mono = Row_nil

let row_cons : Id.t -> mono -> mono -> mono = fun id m rest ->
  Row_cons (id, m, rest)

let row_of_list : (Id.t * mono) list -> mono option -> mono =
    fun fields rest_opt ->
  let cons (id, tp) rest = row_cons id tp rest in
  let nil = Option.default row_nil rest_opt in
  List.fold_right cons fields nil

let row_to_list : mono -> (Id.t * mono) list * mono option = fun row ->
  let rec to_list acc m = match m with
    | Row_nil -> acc, None
    | Row_cons (id, m, rest) -> to_list ((id, m) :: acc) rest
    | _ -> acc, Some m
  in
  let fields, rest = to_list [] row in
  List.rev fields, rest

let scheme : (Id.t * Kind.t) list -> mono -> t = fun quants body ->
  { quants; body }

let scheme_to_mono : t -> mono = fun tp ->
  if tp.quants <> [] then
     expected_mono "scheme_to_mono"
  else
    tp.body

let var_to_string : mono -> string = fun tv -> match tv with
  | Inference_variable (_, id) -> "'" ^ Id.to_string id
  | Variable id -> Id.to_string id
  | _ -> error "var_to_string" "expected variable"

(* Inference *)


module Inferencer : sig

  type state
  val make_state : Kind.Environment.t -> state
  val register : state -> t -> Kind.t -> state
  val to_kind : state -> t -> Kind.t
  val unify : state -> t -> t -> state
  val gen_enter : state -> state
  val gen_exit : state -> t -> state * Kind.t Id.Map.t * t
  val inst : state -> t -> state * t list * t
  val apply : state -> t -> t

end = struct

  module IVE = Inference_variable_environment

  type sub = mono Id.Map.t

  type state = {
    sub : sub ;
    pools : Kind.t IVE.t ;
    kind_env : Kind_env.t ;
  }

  module Sub : sig

    val identity : sub
    val extend : Id.t -> mono -> state -> state
    val apply : mono -> state -> mono

  end = struct

    let identity = Id.Map.empty

    let singleton : Id.t -> mono -> sub = Id.Map.singleton

    let rec apply m sub = match m with
      | Inference_variable (_, id) ->
        Id.Map.find_default m id sub
      | Variable _ ->
        m
      | Application (fn, arg) ->
        app (apply fn sub) (apply arg sub)
      | Row_nil ->
        m
      | Row_cons (id, m, rest) ->
        row_cons id (apply m sub) (apply rest sub)

    let extend id m state =
      let fn m' = apply m' @@ singleton id m in
      { state with sub = Id.Map.add id m @@ Id.Map.map fn state.sub }

    let apply m state = apply m state.sub

  end

  module Pools : sig

    val push : state -> state
    val peek : state -> Kind.t Id.Map.t
    val pop : state -> state
    val insert : Id.t -> Kind.t -> state -> state
    val remove : Id.t -> state -> state
    val update : Id.t -> Id.t -> state -> state
    val get_kind : Id.t -> state -> Kind.t

  end = struct

    let push state = {state with pools = IVE.push state.pools}

    let peek state = IVE.peek state.pools

    let pop state = {state with pools = IVE.pop state.pools}

    let insert id kn state =
      {state with pools = IVE.insert id kn state.pools}

    let remove id state =
      {state with pools = IVE.remove id state.pools}

    let update id1 id2 state =
      {state with pools = IVE.update id1 id2 state.pools}

    let get_kind id state = IVE.find id state.pools

  end

  let raise_occurs : Id.t -> t -> 'a = fun id tp ->
    raise @@ Occurs (id, tp)

  let raise_unify : t -> t -> 'a = fun tp1 tp2 ->
    raise @@ Cannot_unify (tp1, tp2)

  let expected_mono = expected_mono_internal __MODULE__

  let make_state kind_env =
    {sub = Sub.identity; pools = IVE.empty; kind_env}

  let add_kind id kn state =
    {state with kind_env = Kind_env.add id kn state.kind_env}

  let register state tp kn = match tp.body with
    | Inference_variable (_, id) -> Pools.insert id kn state
    | _ -> error "register" "expected variable"

  let apply state tp =
    let body = Sub.apply tp.body state in
    assert (body = Sub.apply body state);
    scheme tp.quants body

  (* Kinding *)

  let to_kind state tp =

    let undefined_id m =
      error "to_kind" @@
        Printf.sprintf "undefined identifier '%s'" (var_to_string m)
    in

    let rec to_kind state m = match m with
      | Inference_variable (Mono, id) ->
        begin try Pools.get_kind id state with
          | Id.Unbound _ -> undefined_id m
        end
      | Inference_variable (Poly, id) | Variable id ->
        begin try Kind_env.find id state.kind_env with
          | Id.Unbound _ -> undefined_id m
        end
      | Application (fn, arg) ->
        let fn_kn = to_kind state fn in
        let fml_arg_kn, res_kn =
          try
            Kind.get_oper fn_kn
          with Invalid_argument _ ->
            error "to_kind" @@
              Printf.sprintf
                "expected function kind; found '%s'"
                (Kind.to_string fn_kn)
        in
        let act_arg_kn = to_kind state arg in
        if Kind.alpha_equivalent act_arg_kn fml_arg_kn then
          res_kn
        else
          error "to_kind" @@
            Printf.sprintf
              "expected kind '%s'; found kind '%s'"
                (Kind.to_string fml_arg_kn)
                (Kind.to_string act_arg_kn)
      | Row_nil ->
        Kind.row
      | Row_cons (_, m, rest) ->
        ignore @@ to_kind state m;
        let rest_kn = to_kind state rest in
        if Kind.alpha_equivalent rest_kn Kind.row then
          Kind.row
        else
          error "to_kind" @@
            Printf.sprintf
              "expected row kind; found kind '%s'"
                (Kind.to_string rest_kn)
    in

    let tp' = apply state tp in
    let add_kind state (q, kn) = add_kind q kn state in
    to_kind (List.fold_left add_kind state tp'.quants) tp'.body

  (* Typing *)

  let unify state tp1 tp2 =

    let rec occurs : Id.t -> mono -> bool = fun id tp -> match tp with
      | Inference_variable (_, id') -> id = id'
      | Variable _ -> false
      | Application (fn, arg) -> occurs id fn || occurs id arg
      | Row_nil -> false
      | Row_cons (_, m, rest) -> occurs id m || occurs id rest
    in

    let rec update_ranks : state -> Id.t -> mono -> state =
        fun state id tp ->
      match tp with
        | Inference_variable (_, id') ->
          Pools.update id' id state
        | Variable _ ->
          state
        | Application (fn, arg) ->
          update_ranks (update_ranks state id fn) id arg
        | Row_nil ->
          state
        | Row_cons (_, m, rest) ->
          update_ranks (update_ranks state id m) id rest
    in

    let merge : state -> Id.t -> mono -> state =
        fun state id m ->
      let state' = update_ranks state id m in
      Sub.extend id m @@ Pools.remove id state'
    in

    let rec unify state m1 m2 =
      let m1' = Sub.apply m1 state in
      let m2' = Sub.apply m2 state in
      match m1', m2' with

        | _, Inference_variable (Poly, _)
        | Inference_variable (Poly, _), _ ->
          expected_mono "unify"

        | Inference_variable (_, id1), Inference_variable (_, id2)
        | Variable id1, Variable id2
            when id1 = id2 ->
          state

        | _, Inference_variable (_, id) ->
          if occurs id m1' then raise_occurs id (scheme tp2.quants m1');
          merge state id m1'
        | Inference_variable (_, id), _ ->
          if occurs id m2' then raise_occurs id (scheme tp2.quants m2');
          merge state id m2'

        | Application (fn1, arg1), Application (fn2, arg2) ->
          unify (unify state fn1 fn2) arg1 arg2

        | Row_nil, Row_nil ->
          state
        | Row_cons (id1, m1, rest1), Row_cons (id2, m2, rest2) ->
          if id1 = id2 then
            unify (unify state m1 m2) rest1 rest2
          else
            let tv = Id.gen_upper () in
            let state = Pools.insert tv Kind.row state in
            let rest = inf_var Mono tv in
            let state = unify state rest1 @@ row_cons id2 m2 rest in
            let state = unify state rest2 @@ row_cons id1 m1 rest in
            state

        | _, _ ->
          raise_unify (scheme tp1.quants m1') (scheme tp2.quants m2')

    in

    if tp1.quants <> [] || tp2.quants <> [] then
          expected_mono "unify";

    let kn1 = to_kind state tp1 in
    let kn2 = to_kind state tp2 in
    if not (Kind.alpha_equivalent kn1 kn2) then
      error "unify" @@
        Printf.sprintf
          "expected '%s'; found '%s'"
          (Kind.to_string kn1)
          (Kind.to_string kn2);

    let state' = unify state tp1.body tp2.body in
    (* TODO: Enable this assertion. *)
    (* assert (apply state' tp1 = apply state' tp2); *)
    state'

  let gen_enter state = Pools.push state

  let gen_exit state tp =

    let free_inf_vars tp =
      let rec free_inf_vars (seen, fvs) tp = match tp with
        | Inference_variable (_, id) ->
          if Id.Set.mem id seen then
            seen, fvs
          else
            Id.Set.add id seen, id :: fvs
        | Variable _ ->
          seen, fvs
        | Application (fn, arg) ->
          free_inf_vars (free_inf_vars (seen, fvs) fn) arg
        | Row_nil ->
          seen, fvs
        | Row_cons (_, m, rest) ->
          free_inf_vars (free_inf_vars (seen, fvs) m) rest
      in
      List.rev @@ snd @@ free_inf_vars (Id.Set.empty, []) tp
    in

    let rec gen qvs tp = match tp with
      | Inference_variable (Mono, id) when Id.Map.mem id qvs ->
        inf_var Poly id
      | Inference_variable (Poly, id) when Id.Map.mem id qvs ->
        assert false
      | Inference_variable _ | Variable _ ->
        tp
      | Application (fn, arg) ->
        app (gen qvs fn) (gen qvs arg)
      | Row_nil ->
        tp
      | Row_cons (id, m, rest) ->
        row_cons id (gen qvs m) (gen qvs rest)
    in

    let tp = apply state tp in

    if tp.quants <> [] then
      expected_mono "gen_exit";

    let qv_kns = Pools.peek state in
    let state' = Pools.pop state in
    let pred id = Id.Map.mem id qv_kns in
    let incl, _ = List.partition pred @@ free_inf_vars tp.body in
    let tp' = {
      quants = List.map (fun q -> q, Id.Map.find q qv_kns) incl;
      body = gen qv_kns tp.body
    } in

    state', qv_kns, tp'

  let inst state tp =

    let rec inst env m = match m with
      | Inference_variable (Mono, id) when Id.Map.mem id env ->
        assert false
      | Inference_variable (Poly, id) ->
        Id.Map.find_default m id env
      | Inference_variable _ | Variable _ ->
        m
      | Application (fn, arg) ->
        app (inst env fn) (inst env arg)
      | Row_nil ->
        row_nil
      | Row_cons (id, m, m') ->
        row_cons id (inst env m) (inst env m')
    in

    let make_var kn (state, tvs) =
      let tv = Id.gen_upper () in
      Pools.insert tv kn state, inf_var Mono tv :: tvs
    in

    let tp = apply state tp in
    let quant_ids, quant_kns = List.split tp.quants in
    let state', vars = List.fold_right make_var quant_kns (state, []) in
    let env = Id.Map.of_list @@ List.combine quant_ids vars in

    state', List.map (scheme []) vars, scheme [] @@ inst env tp.body

end

(* Kinding *)

let to_kind env tp =
  Inferencer.to_kind (Inferencer.make_state env) tp

(* Utilities *)

let to_intl_repr tp =

  let module IR = Records_and_variants.Type in
  let rec to_ir tp = match tp with
    | Inference_variable (_, id) ->
      IR.var id
    | Variable id ->
      IR.var id
    | Application (fn, arg) ->
      IR.app (to_ir fn) (to_ir arg)
    | Row_nil | Row_cons _ ->
      let fields, rest = row_to_list tp in
      let field_to_ir (id, tp) = id, to_ir tp in
      IR.row (List.map field_to_ir fields) (Option.map to_ir rest)
  in

  let quant_to_ir (q, kn) = q, Kind.to_intl_repr kn in
  IR.forall' (List.map quant_to_ir tp.quants) @@ to_ir tp.body

(*
  NOTE: [simplify] does not register the new variables that it creates
  with [Pools], so [simplify]'d types cannot be used with inference
  functions.
 *)
let simplify { quants; body } =

  let fresh =
    let cntr = ref (-1) in
    fun () ->
      incr cntr;
      Id.define @@ Misc.int_to_upper !cntr
  in

  let simplify_id =
    let env = Hashtbl.create 1024 in
    fun id ->
      try Hashtbl.find env id with
        | Not_found ->
          let id' = fresh () in
          Hashtbl.add env id id';
          id'
  in

  let rec simplify tp = match tp with
    | Inference_variable (_, id) ->
      inf_var Mono @@ simplify_id id
    | Variable _ ->
      tp
    | Application (fn, arg) ->
      let fn' = simplify fn in
      let arg' = simplify arg in
      app fn' arg'
    | Row_nil ->
      row_nil
    | Row_cons (id, m, rest) ->
      let m' = simplify m in
      let rest' = simplify rest in
      row_cons id m' rest'
  in

  let quants = List.map (fun (q, kn) -> simplify_id q, kn) quants in
  let body = simplify body in
  { quants; body }

let to_string ?no_simp ?show_quants tp =

  let rec to_string tp =

    let to_paren_string tp = Printf.sprintf "(%s)" (to_string tp) in

    let arg_to_string tp = match tp with
      | Inference_variable (_, _) | Variable _ | Row_nil | Row_cons _ ->
        to_string tp
      | Application (Variable id, _)
          when id = Id.rcrd || id = Id.vrnt ->
        to_string tp
      | Application _ ->
        to_paren_string tp
    in

    let row_to_string row =
      let fields, rest = row_to_list row in
      let field_to_string (id, tp) =
        Printf.sprintf "%s : %s" (Id.to_string id) (to_string tp)
      in
      let fields_str =
        String.concat "; " @@ List.map field_to_string fields
      in
      match rest with
        | None ->
          fields_str
        | Some tp ->
          if String.length fields_str > 0 then
            Printf.sprintf "%s | %s" fields_str (to_string tp)
          else
            Printf.sprintf "%s" (to_string tp)
    in

    match tp with
      | Inference_variable _ | Variable _ ->
        var_to_string tp
      | Application (Application (Variable id as tv, arg), res)
          when id = Id.func ->
        Printf.sprintf "%s %s %s"
          (arg_to_string arg)
          (var_to_string tv)
          (to_string res)
      | Application (Variable id, row) when id = Id.rcrd ->
        Printf.sprintf "{%s}" (row_to_string row)
      | Application (Variable id, row) when id = Id.vrnt ->
        Printf.sprintf "[%s]" (row_to_string row)
      | Application (fn, arg) ->
        Printf.sprintf "%s %s" (to_string fn) (arg_to_string arg)
      | Row_nil | Row_cons _ ->
        Printf.sprintf "<%s>" (row_to_string tp)
  in

  let { quants; body } = if no_simp = None then simplify tp else tp in
  if quants = [] || show_quants = None then
    to_string body
  else
    let quant_to_string (q, kn) =
      Printf.sprintf "'%s :: %s" (Id.to_string q) (Kind.to_string kn)
    in
    Printf.sprintf "forall %s . %s"
      (String.concat " . forall " @@ List.map quant_to_string quants)
      (to_string body)

(* External functions *)

let func arg res =
  let func arg res = List.fold_left app (var Id.func) [arg; res] in
  match arg.quants, res.quants with
    | [], [] -> scheme [] @@ func arg.body res.body
    | _ :: _, _ | _, _ :: _ -> expected_mono "func"

let func' args res = List.fold_right func args res

let rcrd fields rest =
  let field_to_mono (id, tp) = id, scheme_to_mono tp in
  let fields' = List.map field_to_mono fields in
  let rest' = Option.map scheme_to_mono rest in
  scheme [] @@ app (var Id.rcrd) (row_of_list fields' rest')

let vrnt cases rest =
  let case_to_mono (id, tp) = id, scheme_to_mono tp in
  let cases' = List.map case_to_mono cases in
  let rest' = Option.map scheme_to_mono rest in
  scheme [] @@ app (var Id.vrnt) (row_of_list cases' rest')

let inf_var id = scheme [] @@ inf_var Mono id

let get_quants { quants; _ } = quants
