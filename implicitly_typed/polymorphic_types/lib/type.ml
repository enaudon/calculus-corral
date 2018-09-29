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

(* Exceptions *)

exception Occurs of Id.t * t

(* Internal functions *)

let error : string -> string -> 'a = fun fn_name msg ->
  failwith @@ Printf.sprintf "%s.%s: %s" __MODULE__ fn_name msg

let expected_mono : string -> 'a = fun fn_name ->
  invalid_arg @@
    Printf.sprintf "%s.%s: expected monomorphic type" __MODULE__ fn_name

let raise_occurs : Id.t -> t -> 'a = fun id tp ->
  raise @@ Occurs (id, tp)

let var : Id.t -> mono = fun id -> Variable id

let func : mono -> mono -> mono = fun arg res -> Function (arg, res)

let scheme : Id.t list -> mono -> t = fun quants body ->
  { quants; body }

(* Inference *)

module State : sig

  (* External interface *)

  type s
  val initial : s
  val apply_solution : t -> s -> t

  (* Internal components *)

  module Sub : sig
    val extend_mono : Id.t -> mono -> s -> s
    val apply : t -> s -> t
    val apply_mono : mono -> s -> mono
  end

  module Pools : sig
    val push : s -> s
    val peek : s -> Id.Set.t
    val pop : s -> s
    val register : s -> Id.t -> s
    val unregister : s -> Id.t -> s
    val update : s -> Id.t -> Id.t -> s
    val is_mono : s -> Id.t -> bool
  end

end = struct

  type sub = mono Id.Map.t

  type s = {
    sub : sub ;
    pools : Rank.Pools.p ;
  }

  module Sub = struct

    let identity = Id.Map.empty

    let singleton : Id.t -> mono -> sub = Id.Map.singleton

    let rec apply_mono m sub = match m with
      | Variable id ->
        Id.Map.find_default m id sub
      | Function (arg, res) ->
        func (apply_mono arg sub) (apply_mono res sub)

    let extend_mono id m state =
      let fn m' = apply_mono m' @@ singleton id m in
      { state with sub = Id.Map.add id m @@ Id.Map.map fn state.sub }

    let apply tp { sub; _ } =
      let body = apply_mono tp.body sub in
      assert (body = apply_mono body sub);
      scheme tp.quants body

    let apply_mono m state = apply_mono m state.sub

  end

  module Pools = struct

    module Ps = Rank.Pools

    let push state = {state with pools = Ps.push state.pools}

    let peek state = Ps.peek state.pools

    let pop state = {state with pools = Ps.pop state.pools}

    let register state id =
      {state with pools = Ps.register state.pools id}

    let unregister state id =
      {state with pools = Ps.unregister state.pools id}

    let update state id1 id2 =
      {state with pools = Ps.update state.pools id1 id2}

    let is_mono state id = Ps.is_mono state.pools id

  end

  let initial = {
    sub = Sub.identity;
    pools = Rank.Pools.empty ;
  }

  let apply_solution = Sub.apply

end

let unify state tp1 tp2 =

  let rec occurs : Id.t -> mono -> bool = fun id tp -> match tp with
    | Variable id' -> id = id'
    | Function (arg, res) -> occurs id arg || occurs id res
  in

  let rec update_ranks
      : State.s -> Id.t -> mono -> State.s
      = fun state id tp ->
    match tp with
      | Variable id' ->
        State.Pools.update state id' id
      | Function (arg, res) ->
        update_ranks (update_ranks state id arg) id res
  in

  let merge
      : State.s -> Id.t -> mono -> State.s
      = fun state id m ->
    let state' = update_ranks state id m in
    State.Sub.extend_mono id m @@ State.Pools.unregister state' id
  in

  let rec unify state m1 m2 =
    let m1' = State.Sub.apply_mono m1 state in
    let m2' = State.Sub.apply_mono m2 state in
    match m1', m2' with
      | Variable id, _ when not @@ State.Pools.is_mono state id ->
        expected_mono "unify"
      | _, Variable id when not @@ State.Pools.is_mono state id ->
        expected_mono "unify"
      | Variable id1, Variable id2 when id1 = id2 ->
        state
      | Variable id, _ ->
        if occurs id m2' then raise_occurs id (scheme tp2.quants m2');
        merge state id m2'
      | _, Variable id ->
        if occurs id m1' then raise_occurs id (scheme tp2.quants m1');
        merge state id m1'
      | Function (arg1, res1), Function (arg2, res2) ->
        unify (unify state arg1 arg2) res1 res2
  in

  if tp1.quants <> [] || tp2.quants <> [] then
        expected_mono "unify";

  let state' = unify state tp1.body tp2.body in
  assert (State.Sub.apply tp1 state' = State.Sub.apply tp2 state');
  state'

let register state m = match m with
  | Variable id -> State.Pools.register state id
  | _ -> error "register" "expected variable"

let gen_enter state = State.Pools.push state

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

  let tp = State.Sub.apply tp state in

  if tp.quants <> [] then
    expected_mono "gen_exit";

  let qvs = State.Pools.peek state in
  let state' = State.Pools.pop state in
  let pred id = Id.Set.mem id qvs in
  let incl, _ = List.partition pred @@ free_vars tp.body in
  let tp' = { quants = incl; body = tp.body } in

  state', qvs, tp'

let inst state tp =

  let tp = State.Sub.apply tp state in

  let quants = tp.quants in
  let vars = List.map (fun _ -> var @@ Id.fresh_upper ()) quants in
  let state' = List.fold_left register state vars in
  let env = Id.Map.of_list @@ List.combine quants vars in

  let rec inst m = match m with
    | Variable id -> Id.Map.find_default m id env
    | Function (arg, res) -> func (inst arg) (inst res)
  in

  state', List.map (scheme []) vars, scheme [] @@ inst tp.body

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
      Id.of_string @@ Misc.int_to_upper !cntr
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

let register state tp = register state tp.body
