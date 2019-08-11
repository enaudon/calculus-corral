module Id = Identifier
module Misc = Miscellaneous
module Opt = Option

type t =
  | Inference_variable of Id.t
  | Function of t * t
  | Universal of Id.t * t

module Environment = Type_environment.Make (struct
  type value = t
  let initial_types = []
  let initial_terms = []
end)

(* Exceptions *)

exception Occurs of Id.t * t

(* Internal utilities *)

let raise_poly : string -> 'a = fun fn_name ->
  invalid_arg @@
    Printf.sprintf
      "%s.%s: unexpected polymorphic type"
      __MODULE__
      fn_name

(* Constructors *)

let inf_var id = Inference_variable id

let func arg res = Function (arg, res)

let forall : Id.t -> t -> t = fun quant body ->
  Universal (quant, body)

let forall' : Id.t list -> t -> t = fun quants body ->
  List.fold_right forall quants body

(* Destructors *)

let get_forall' : t -> Identifier.t list * t = fun tp ->
  let rec get_forall acc tp = match tp with
    | Universal (quant, body) -> get_forall (quant :: acc) body
    | _ -> acc, tp
  in
  let quants, tp = get_forall [] tp in
  List.rev quants, tp

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

  type sub = t Id.Map.t

  type state = {
    sub : sub;
    pools : unit IVE.t;
  }

  module Sub : sig

    val identity : sub
    val extend : Id.t -> t -> state -> state
    val apply : t -> state -> t

  end = struct

    let identity = Id.Map.empty

    let singleton : Id.t -> t -> sub = Id.Map.singleton

    let rec apply tp sub = match tp with
      | Inference_variable id -> Id.Map.find_default tp id sub
      | Function (arg, res) -> func (apply arg sub) (apply res sub)
      | Universal (quant, body) -> forall quant @@ apply body sub

    let extend id tp state =
      let apply tp' = apply tp' @@ singleton id tp in
      { state with
        sub = Id.Map.add id tp @@ Id.Map.map apply state.sub }

    let apply tp state =
      let tp' = apply tp state.sub in
      assert (tp' = apply tp' state.sub);
      tp'

  end

  module Pools : sig

    val push : state -> state
    val peek : state -> Id.Set.t
    val pop : state -> state
    val insert : Id.t -> state -> state
    val remove : Id.t -> state -> state
    val update : Id.t -> Id.t -> state -> state

  end = struct

    let push state = {state with pools = IVE.push state.pools}

    let peek state =
      IVE.peek state.pools
        |> Id.Map.keys
        |> Id.Set.of_list

    let pop state = {state with pools = IVE.pop state.pools}

    let insert id state =
      {state with pools = IVE.insert id () state.pools}

    let remove id state =
      {state with pools = IVE.remove id state.pools}

    let update id1 id2 state =
      {state with pools = IVE.update id1 id2 state.pools}

  end

  let error : string -> string -> 'a = fun fn_name msg ->
    failwith @@ Printf.sprintf "%s.%s: %s" __MODULE__ fn_name msg

  let raise_occurs : Id.t -> t -> 'a = fun id tp ->
    raise @@ Occurs (id, tp)

  let initial = {
    sub = Sub.identity ;
    pools = IVE.empty ;
  }

  let register state tp = match tp with
    | Inference_variable id -> Pools.insert id state
    | _ -> error "register" "expected variable"

  let unify state tp1 tp2 =

    let rec occurs : Id.t -> t -> bool = fun id tp -> match tp with
      | Inference_variable id' -> id = id'
      | Function (arg, res) -> occurs id arg || occurs id res
      | Universal _ -> raise_poly "Inferencer.unify.occurs"
    in

    let rec update_ranks : state -> Id.t -> t -> state =
        fun state id tp ->
      match tp with
        | Inference_variable id' ->
          Pools.update id' id state
        | Function (arg, res) ->
          update_ranks (update_ranks state id arg) id res
        | Universal _ ->
          raise_poly "Inferencer.unify.update_ranks"
    in

    let merge : state -> Id.t -> t -> state =
        fun state id tp ->
      let state' = update_ranks state id tp in
      Sub.extend id tp @@ Pools.remove id state'
    in

    let rec unify state tp1 tp2 =
      let tp1' = Sub.apply tp1 state in
      let tp2' = Sub.apply tp2 state in
      match tp1', tp2' with

        | _, Universal _ | Universal _, _ ->
          raise_poly "Inferencer.unify.unify";

        | Inference_variable id1, Inference_variable id2
            when id1 = id2 ->
          state

        | _, Inference_variable id ->
          if occurs id tp1' then raise_occurs id tp1';
          merge state id tp1'
        | Inference_variable id, _ ->
          if occurs id tp2' then raise_occurs id tp2';
          merge state id tp2'

        | Function (arg1, res1), Function (arg2, res2) ->
          unify (unify state arg1 arg2) res1 res2

    in

    let state' = unify state tp1 tp2 in
    assert (Sub.apply tp1 state' = Sub.apply tp2 state');
    state'

  let gen_enter state = Pools.push state

  let gen_exit state tp =

    let free_inf_vars tp =

      let rec free_inf_vars (seen, fvs) tp = match tp with
        | Inference_variable id when not @@ Id.Set.mem id seen ->
          (Id.Set.add id seen, id :: fvs)
        | Inference_variable _ ->
          (seen, fvs)
        | Function (arg, res) ->
          free_inf_vars (free_inf_vars (seen, fvs) arg) res
        | Universal _ ->
          raise_poly "Inferencer.gen_exit.free_inf_vars"
      in

      tp
        |> free_inf_vars (Id.Set.empty, [])
        |> snd
        |> List.rev

    in

    let tp' = Sub.apply tp state in
    let qvs = Pools.peek state in
    let pred id = Id.Set.mem id qvs in
    let incl = List.filter pred @@ free_inf_vars tp' in

    (Pools.pop state, qvs, forall' incl tp')

  let inst state tp =

    let rec inst env tp = match tp with
      | Inference_variable id -> Id.Map.find_default tp id env
      | Function (arg, res) -> func (inst env arg) (inst env res)
      | Universal _ -> raise_poly "Inferencer.inst"
    in

    let make_var _ (state, tvs) =
      let tv = Id.gen_upper () in
      (Pools.insert tv state, inf_var tv :: tvs)
    in

    let quants, tp' = get_forall' @@ Sub.apply tp state in
    let state', vars = List.fold_right make_var quants (state, []) in
    let env = Id.Map.of_list @@ List.combine quants vars in

    (state', vars, inst env tp')

  let apply state tp = Sub.apply tp state

end

(* Utilities *)

let rec to_intl_repr tp =
  let module IR = Universal_types.Type in
  match tp with
    | Inference_variable id ->
      IR.var id
    | Function (arg, res) ->
      IR.func (to_intl_repr arg) (to_intl_repr res)
    | Universal (quant, body) ->
      IR.forall quant @@ to_intl_repr body

(*
  NOTE: [simplify] does not register the new variables that it creates
  with [Pools], so [simplify]'d types cannot be used with inference
  functions.
 *)
let simplify tp =

  let fresh =
    let cntr = ref (-1) in
    fun () ->
      incr cntr;
      Id.define @@ Misc.int_to_upper !cntr
  in

  let rec simplify env tp = match tp with
    | Inference_variable id ->
      inf_var @@ Id.Map.find id env
    | Function (arg, res) ->
      let arg' = simplify env arg in
      let res' = simplify env res in
      func arg' res'
    | Universal (quant, body) ->
      let quant' = fresh () in
      forall quant' @@ simplify (Id.Map.add quant quant' env) body
  in

  simplify (Id.Map.empty) tp

let to_string ?no_simp ?show_quants tp =

  let rec to_paren_string tp =
    Printf.sprintf "(%s)" (to_string tp)

  and to_string tp = match tp with
    | Inference_variable id ->
      Printf.sprintf "'%s" (Id.to_string id)
    | Function (arg, res) ->
      let arg_to_string tp = match tp with
        | Inference_variable _ -> to_string tp
        | Function _ | Universal _ -> to_paren_string tp
      in
      Printf.sprintf "%s -> %s" (arg_to_string arg) (to_string res)
    | Universal (quant, body) ->
      if show_quants = Opt.none then
        to_string body
      else
        Printf.sprintf "forall %s . %s"
          (Id.to_string quant)
          (to_string body)
  in

  to_string @@ if no_simp = Opt.none then simplify tp else tp

(* External functions *)

let func' args res = List.fold_right func args res

let get_quants tp = fst @@ get_forall' tp
