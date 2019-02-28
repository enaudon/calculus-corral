module Id = Identifier
module Misc = Miscellaneous

(* The type of monomorphic types. *)
type mono =
  | Constant of Id.t
  | Variable of Id.t
  | Application of mono * mono

(* The type of types schemes. *)
type t = {
  quants : (Id.t * Kind.t) list ;
  body : mono ;
}

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

let cst : Id.t -> mono = fun id -> Constant id

let var : Id.t -> mono = fun id -> Variable id

let app : mono -> mono -> mono = fun fn arg -> Application (fn, arg)

let scheme : (Id.t * Kind.t) list -> mono -> t = fun quants body ->
  { quants; body }

let func_id = Id.define "->"

(* Kinding *)

let default_env =
  let prop = Kind.prop in
  let oper' = Kind.oper' in
  Id.Map.empty |>
    Id.Map.add func_id (oper' [prop; prop] prop)

let to_kind env tp =

  let rec to_kind env m = match m with
    | Constant id | Variable id ->
      begin try Id.Map.find id env with
        | Id.Unbound id ->
          error "to_kind" @@
            Printf.sprintf "undefined identifier '%s'" (Id.to_string id)
      end
    | Application (fn, arg) ->
      let fn_kn = to_kind env fn in
      let fml_arg_kn, res_kn =
        try
          Kind.get_oper fn_kn
        with Invalid_argument _ ->
          error "to_kind" @@
            Printf.sprintf
              "expected function kind; found '%s'"
              (Kind.to_string fn_kn)
      in
      let act_arg_kn = to_kind env arg in
      if Kind.alpha_equivalent act_arg_kn fml_arg_kn then
        res_kn
      else
        error "to_kind" @@
          Printf.sprintf
            "expected kind '%s'; found kind '%s'"
              (Kind.to_string fml_arg_kn)
              (Kind.to_string act_arg_kn)
  in

  let ext_env env (q, kn) = Id.Map.add q kn env in
  to_kind (List.fold_left ext_env env tp.quants) tp.body

(* Inference *)


module Inferencer : sig

  type state
  val initial : state
  val register : ?rigid : unit -> state -> t -> Kind.t -> state
  val unify : state -> t -> t -> state
  val gen_enter : state -> state
  val gen_exit : state -> t -> state * Kind.t Id.Map.t * t
  val inst : state -> t -> state * t list * t
  val apply : state -> t -> t

end = struct

  module IVE = Inference_variable_environment

  type sub = mono Id.Map.t

  type rigidity =
    | Flexible
    | Rigid

  type state = {
    sub : sub ;
    pools : (Kind.t * rigidity) IVE.t ;
  }

  module Sub : sig

    val identity : sub
    val extend : Id.t -> mono -> state -> state
    val apply : mono -> state -> mono

  end = struct

    let identity = Id.Map.empty

    let singleton : Id.t -> mono -> sub = Id.Map.singleton

    let rec apply m sub = match m with
      | Constant _ ->
        m
      | Variable id ->
        Id.Map.find_default m id sub
      | Application (fn, arg) ->
        app (apply fn sub) (apply arg sub)

    let extend id m state =
      let fn m' = apply m' @@ singleton id m in
      { state with sub = Id.Map.add id m @@ Id.Map.map fn state.sub }

    let apply m state = apply m state.sub

  end

  module Pools : sig

    val push : state -> state
    val peek : state -> Kind.t Id.Map.t
    val pop : state -> state
    val insert : Id.t -> Kind.t -> bool -> state -> state
    val remove : Id.t -> state -> state
    val update : Id.t -> Id.t -> state -> state
    val is_mono : Id.t -> state -> bool
    val is_rigid : Id.t -> state -> bool

  end = struct

    let push state = {state with pools = IVE.push state.pools}

    let peek state = Id.Map.map fst @@ IVE.peek state.pools

    let pop state = {state with pools = IVE.pop state.pools}

    let insert id kn is_rigid state =
      let rigidity = if is_rigid then Rigid else Flexible in
      {state with pools = IVE.insert id (kn, rigidity) state.pools}

    let remove id state =
      {state with pools = IVE.remove id state.pools}

    let update id1 id2 state =
      {state with pools = IVE.update id1 id2 state.pools}

    let is_mono id state = IVE.is_mono id state.pools

    let is_rigid id state = match snd @@ IVE.find id state.pools with
      | Flexible -> false
      | Rigid -> true

  end

  let raise_occurs : Id.t -> t -> 'a = fun id tp ->
    raise @@ Occurs (id, tp)

  let raise_unify : t -> t -> 'a = fun tp1 tp2 ->
    raise @@ Cannot_unify (tp1, tp2)

  let expected_mono = expected_mono_internal __MODULE__

  let initial = {
    sub = Sub.identity ;
    pools = IVE.empty ;
  }

  let apply state tp =
    let body = Sub.apply tp.body state in
    assert (body = Sub.apply body state);
    scheme tp.quants body

  let register ?rigid state tp kn = match tp.body with
    | Variable id -> Pools.insert id kn (rigid <> None) state
    | _ -> error "register" "expected variable"

  let unify state tp1 tp2 =

    let rec occurs : Id.t -> mono -> bool = fun id tp -> match tp with
      | Constant _ -> false
      | Variable id' -> id = id'
      | Application (fn, arg) -> occurs id fn || occurs id arg
    in

    let rec update_ranks : state -> Id.t -> mono -> state =
        fun state id tp ->
      match tp with
        | Constant _ ->
          state
        | Variable id' ->
          Pools.update id' id state
        | Application (fn, arg) ->
          update_ranks (update_ranks state id fn) id arg
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

        | _, Variable id when not @@ Pools.is_mono id state ->
          expected_mono "unify"
        | Variable id, _ when not @@ Pools.is_mono id state ->
          expected_mono "unify"

        | Constant id1, Constant id2
        | Variable id1, Variable id2 when id1 = id2 ->
          state

        | Variable _, Variable id when Id.is_generated id ->
          if occurs id m1' then raise_occurs id (scheme tp2.quants m1');
          merge state id m1'
        | Variable id, Variable _ when Id.is_generated id ->
          if occurs id m2' then raise_occurs id (scheme tp2.quants m2');
          merge state id m2'

        | _, Variable id when not @@ Pools.is_rigid id state ->
          if occurs id m1' then raise_occurs id (scheme tp2.quants m1');
          merge state id m1'
        | Variable id, _ when not @@ Pools.is_rigid id state ->
          if occurs id m2' then raise_occurs id (scheme tp2.quants m2');
          merge state id m2'

        | Application (fn1, arg1), Application (fn2, arg2) ->
          unify (unify state fn1 fn2) arg1 arg2

        | _, _ ->
          raise_unify (scheme tp1.quants m1') (scheme tp2.quants m2')

    in

    if tp1.quants <> [] || tp2.quants <> [] then
          expected_mono "unify";

    let state' = unify state tp1.body tp2.body in
    (* TODO: Enable this assertion. *)
    (* assert (apply state' tp1 = apply state' tp2); *)
    state'

  let gen_enter state = Pools.push state

  let gen_exit state tp =

    let free_vars tp =
      let rec free_vars (seen, fvs) tp = match tp with
        | Constant _ ->
          seen, fvs
        | Variable id ->
          if Id.Set.mem id seen then
            seen, fvs
          else
            Id.Set.add id seen, id :: fvs
        | Application (fn, arg) ->
          free_vars (free_vars (seen, fvs) fn) arg
      in
      List.rev @@ snd @@ free_vars (Id.Set.empty, []) tp
    in

    let tp = apply state tp in

    if tp.quants <> [] then
      expected_mono "gen_exit";

    let qv_kns = Pools.peek state in
    let state' = Pools.pop state in
    let pred id = Id.Map.mem id qv_kns in
    let incl, _ = List.partition pred @@ free_vars tp.body in
    let tp' = {
      quants = List.map (fun q -> q, Id.Map.find q qv_kns) incl;
      body = tp.body
    } in

    state', qv_kns, tp'

  let inst state tp =

    let rec inst env m = match m with
      | Constant _ -> m
      | Variable id -> Id.Map.find_default m id env
      | Application (fn, arg) -> app (inst env fn) (inst env arg)
    in

    let make_var kn (state, tvs) =
      let tv = Id.gen_upper () in
      Pools.insert tv kn false state, var tv :: tvs
    in

    let tp = apply state tp in
    let quant_ids, quant_kns = List.split tp.quants in
    let state', vars = List.fold_right make_var quant_kns (state, []) in
    let env = Id.Map.of_list @@ List.combine quant_ids vars in

    state', List.map (scheme []) vars, scheme [] @@ inst env tp.body

end

(* Utilities *)

let to_intl_repr tp =

  let module IR = Type_operators.Type in
  let rec to_ir tp = match tp with
    | Constant id -> IR.var id
    | Variable id -> IR.var id
    | Application (fn, arg) -> IR.app (to_ir fn) (to_ir arg)
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
    | Variable id when Id.is_generated id ->
      var @@ simplify_id id
    | Constant _ | Variable _ ->
      tp
    | Application (fn, arg) ->
      let fn' = simplify fn in
      let arg' = simplify arg in
      app fn' arg'
  in

  let quants = List.map (fun (q, kn) -> simplify_id q, kn) quants in
  let body = simplify body in
  { quants; body }

let to_string ?no_simp ?show_quants tp =

  let rec to_string tp =

    let to_paren_string tp = Printf.sprintf "(%s)" (to_string tp) in

    let arg_to_string tp = match tp with
      | Constant _ | Variable _ -> to_string tp
      | Application _ -> to_paren_string tp
    in

    match tp with
      | Constant id | Variable id ->
        Id.to_string id
      | Application (Application (Constant id, arg), res)
          when id = func_id ->
        Printf.sprintf "%s %s %s"
          (arg_to_string arg)
          (Id.to_string func_id)
          (to_string res)
      | Application (fn, arg) ->
        Printf.sprintf "%s %s" (to_string fn) (arg_to_string arg)
  in

  let { quants; body } = if no_simp = None then simplify tp else tp in
  if quants = [] || show_quants = None then
    to_string body
  else
    let quant_to_string (q, kn) =
      Printf.sprintf "%s :: %s" (Id.to_string q) (Kind.to_string kn)
    in
    Printf.sprintf "forall %s . %s"
      (String.concat " . forall " @@ List.map quant_to_string quants)
      (to_string body)

(* External functions *)

let var id = scheme [] @@ var id

let func arg res =
  let func arg res = List.fold_left app (cst func_id) [arg; res] in
  match arg.quants, res.quants with
    | [], [] -> scheme [] @@ func arg.body res.body
    | _ :: _, _ | _, _ :: _ -> expected_mono "func"

let func' args res = List.fold_right func args res

let get_quants { quants; _ } = quants
