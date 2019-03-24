module Id = Identifier
module Misc = Miscellaneous

(* The type of monomorphic types. *)
type mono =
  | Variable of Id.t
  | Function of mono * mono

(* The type of types schemes. *)
type t = {
  quants : Id.t list ;
  body : mono ;
}

module Environment = Type_environment.Make (struct
  type value = t
  let initial_types = []
  let initial_terms = []
end)

(* Exceptions *)

exception Occurs of Id.t * t

(* Internal functions *)

let expected_mono_internal : string -> string -> 'a =
    fun modl_name fn_name ->
  invalid_arg @@
    Printf.sprintf "%s.%s: expected monomorphic type" modl_name fn_name

let expected_mono = expected_mono_internal __MODULE__

let var : Id.t -> mono = fun id -> Variable id

let func : mono -> mono -> mono = fun arg res -> Function (arg, res)

let scheme : Id.t list -> mono -> t = fun quants body ->
  { quants; body }

(* Inference *)

module Inferencer : sig

  type state
  val initial : state
  val register : state -> t -> state
  val unify : state -> t -> t -> state
  val gen_enter : state -> state
  val gen_exit : state -> t -> state * Id.Set.t * t
  val inst : state -> t -> state * t list * t
  val apply : state -> t -> t

end = struct

  module IVE = Inference_variable_environment

  type sub = mono Id.Map.t

  type state = {
    sub : sub ;
    pools : unit IVE.t ;
  }

  module Sub : sig

    val identity : sub
    val extend : Id.t -> mono -> state -> state
    val apply : mono -> state -> mono

  end = struct

    let identity = Id.Map.empty

    let singleton : Id.t -> mono -> sub = Id.Map.singleton

    let rec apply m sub = match m with
      | Variable id ->
        Id.Map.find_default m id sub
      | Function (arg, res) ->
        func (apply arg sub) (apply res sub)

    let extend id m state =
      let fn m' = apply m' @@ singleton id m in
      { state with sub = Id.Map.add id m @@ Id.Map.map fn state.sub }

    let apply m state = apply m state.sub

  end

  module Pools : sig

    val push : state -> state
    val peek : state -> Id.Set.t
    val pop : state -> state
    val register : Id.t -> state -> state
    val unregister : Id.t -> state -> state
    val update : Id.t -> Id.t -> state -> state
    val is_mono : Id.t -> state -> bool

  end = struct

    let push state = {state with pools = IVE.push state.pools}

    let peek state =
      IVE.peek state.pools |> Id.Map.keys |> Id.Set.of_list

    let pop state = {state with pools = IVE.pop state.pools}

    let register id state =
      {state with pools = IVE.insert id () state.pools}

    let unregister id state =
      {state with pools = IVE.remove id state.pools}

    let update id1 id2 state =
      {state with pools = IVE.update id1 id2 state.pools}

    let is_mono id state = IVE.is_mono id state.pools

  end

  let error : string -> string -> 'a = fun fn_name msg ->
    failwith @@ Printf.sprintf "%s.%s: %s" __MODULE__ fn_name msg

  let raise_occurs : Id.t -> t -> 'a = fun id tp ->
    raise @@ Occurs (id, tp)

  let expected_mono = expected_mono_internal __MODULE__

  let initial = {
    sub = Sub.identity ;
    pools = IVE.empty ;
  }

  let apply state tp =
    let body = Sub.apply tp.body state in
    assert (body = Sub.apply body state);
    scheme tp.quants body

  let register state tp = match tp.body with
    | Variable id -> Pools.register id state
    | _ -> error "register" "expected variable"

  let unify state tp1 tp2 =

    let rec occurs : Id.t -> mono -> bool = fun id tp -> match tp with
      | Variable id' -> id = id'
      | Function (arg, res) -> occurs id arg || occurs id res
    in

    let rec update_ranks : state -> Id.t -> mono -> state =
        fun state id tp ->
      match tp with
        | Variable id' ->
          Pools.update id' id state
        | Function (arg, res) ->
          update_ranks (update_ranks state id arg) id res
    in

    let merge : state -> Id.t -> mono -> state =
        fun state id m ->
      let state' = update_ranks state id m in
      Sub.extend id m @@ Pools.unregister id state'
    in

    let rec unify state m1 m2 =
      let m1' = Sub.apply m1 state in
      let m2' = Sub.apply m2 state in
      match m1', m2' with

        | _, Variable id when not @@ Pools.is_mono id state ->
          expected_mono "unify"
        | Variable id, _ when not @@ Pools.is_mono id state ->
          expected_mono "unify"

        | Variable id1, Variable id2 when id1 = id2 ->
          state

        | _, Variable id ->
          if occurs id m1' then raise_occurs id (scheme tp2.quants m1');
          merge state id m1'
        | Variable id, _ ->
          if occurs id m2' then raise_occurs id (scheme tp2.quants m2');
          merge state id m2'

        | Function (arg1, res1), Function (arg2, res2) ->
          unify (unify state arg1 arg2) res1 res2

    in

    if tp1.quants <> [] || tp2.quants <> [] then
          expected_mono "unify";

    let state' = unify state tp1.body tp2.body in
    assert (apply state' tp1 = apply state' tp2);
    state'

  let gen_enter state = Pools.push state

  let gen_exit state tp =

    let free_vars tp =
      let rec free_vars (seen, fvs) tp = match tp with
        | Variable id ->
          if Id.Set.mem id seen then
            seen, fvs
          else
            Id.Set.add id seen, id :: fvs
        | Function (arg, res) ->
          free_vars (free_vars (seen, fvs) arg) res
      in
      List.rev @@ snd @@ free_vars (Id.Set.empty, []) tp
    in

    let tp = apply state tp in

    if tp.quants <> [] then
      expected_mono "gen_exit";

    let qvs = Pools.peek state in
    let state' = Pools.pop state in
    let pred id = Id.Set.mem id qvs in
    let incl, _ = List.partition pred @@ free_vars tp.body in
    let tp' = { quants = incl; body = tp.body } in

    state', qvs, tp'

  let inst state tp =

    let rec inst env m = match m with
      | Variable id -> Id.Map.find_default m id env
      | Function (arg, res) -> func (inst env arg) (inst env res)
    in

    let make_var _ (state, tvs) =
      let tv = Id.gen_upper () in
      Pools.register tv state, var tv :: tvs
    in

    let tp = apply state tp in
    let state', vars = List.fold_right make_var tp.quants (state, []) in
    let env = Id.Map.of_list @@ List.combine tp.quants vars in

    state', List.map (scheme []) vars, scheme [] @@ inst env tp.body

end

(* Utilities *)

let to_intl_repr tp =

  let module IR = Universal_types.Type in
  let rec to_ir tp = match tp with
    | Variable id -> IR.var id
    | Function (arg, res) -> IR.func (to_ir arg) (to_ir res)
  in

  IR.forall' tp.quants @@ to_ir tp.body

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
    | Variable id ->
      var @@ simplify_id id
    | Function (arg, res) ->
      let arg' = simplify arg in
      let res' = simplify res in
      func arg' res'
  in

  let quants = List.map simplify_id quants in
  let body = simplify body in
  { quants; body }

let to_string ?no_simp ?show_quants tp =

  let rec to_string tp =
    let to_paren_string tp = Printf.sprintf "(%s)" (to_string tp) in
    match tp with
      | Variable id ->
        Id.to_string id
      | Function (arg, res) ->
        let arg_to_string tp = match tp with
          | Variable _ -> to_string tp
          | Function _ -> to_paren_string tp
        in
        Printf.sprintf "%s -> %s" (arg_to_string arg) (to_string res)
  in

  let { quants; body } = if no_simp = None then simplify tp else tp in
  if quants = [] || show_quants = None then
    to_string body
  else
    Printf.sprintf "forall %s . %s"
      (String.concat " . forall " @@ List.map Id.to_string quants)
      (to_string body)

(* External functions *)

let var id = scheme [] @@ var id

let func arg res = match arg.quants, res.quants with
  | [], [] -> scheme [] @@ func arg.body res.body
  | _ :: _, _ | _, _ :: _ -> expected_mono "func"

let func' args res = List.fold_right func args res

let get_quants { quants; _ } = quants
