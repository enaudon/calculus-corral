module Id = Identifier

module type Input = sig
  type value
  val initial_types : (Identifier.t * value) list
  val initial_terms : (Identifier.t * value) list
end

module type Output = sig

  type value
  type t

  val empty : t
  val initial : t

  val singleton_type : Identifier.t -> value -> t
  val add_type : Identifier.t -> value -> t -> t
  val del_type : Identifier.t -> t -> t
  val find_type : Identifier.t -> t -> value
  val find_default_type : value -> Identifier.t -> t -> value
  val mem_type : Identifier.t -> t -> bool
  val type_bindings : t -> (Identifier.t * value) list
  val type_keys : t -> Identifier.t list
  val type_values : t -> value list
  val fold_type :
    (Identifier.t -> value -> 'a -> 'a) -> t -> 'a -> 'a

  val singleton_term : Identifier.t -> value -> t
  val add_term : Identifier.t -> value -> t -> t
  val del_term : Identifier.t -> t -> t
  val find_term : Identifier.t -> t -> value
  val find_default_term : value -> Identifier.t -> t -> value
  val mem_term : Identifier.t -> t -> bool
  val term_bindings : t -> (Identifier.t * value) list
  val term_keys : t -> Identifier.t list
  val term_values : t -> value list
  val fold_term :
    (Identifier.t -> value -> 'a -> 'a) -> t -> 'a -> 'a

  val of_list :
    (Identifier.t * value) list -> (Identifier.t * value) list -> t
  val bindings : t -> (Identifier.t * value) list
  val fold_both :
    (Identifier.t -> value -> 'a -> 'a) ->
    (Identifier.t -> value -> 'a -> 'a) ->
    t ->
    'a ->
    'a

end

module Make (Input : Input) = struct

  open Id.Map

  type t = {
    type_ids : Input.value Id.Map.t;
    term_ids : Input.value Id.Map.t;
  }

  let lift_type_fn fn env = {env with type_ids = fn env.type_ids}

  let lift_term_fn fn env = {env with term_ids = fn env.term_ids}

  let empty = {type_ids = Id.Map.empty; term_ids = Id.Map.empty}

  let add_type id v = lift_type_fn @@ add id v
  let singleton_type id tp = add_type id tp empty
  let del_type id = lift_type_fn @@ del id
  let find_type id env = find id env.type_ids
  let find_default_type v id env = find_default v id env.type_ids
  let mem_type id env = mem id env.type_ids
  let type_bindings env = bindings env.type_ids
  let type_keys env = keys env.type_ids
  let type_values env = values env.type_ids

  let add_term id v = lift_term_fn @@ add id v
  let singleton_term id tp = add_term id tp empty
  let del_term id = lift_term_fn @@ del id
  let find_term id env = find id env.term_ids
  let find_default_term v id env = find_default v id env.term_ids
  let mem_term id env = mem id env.term_ids
  let term_bindings env = bindings env.term_ids
  let term_keys env = keys env.term_ids
  let term_values env = values env.term_ids

  let of_list tps tms =
    empty |>
      List.fold_right (fun (id, v) -> add_type id v) tps |>
      List.fold_right (fun (id, v) -> add_term id v) tms

  let bindings env = type_bindings env @ term_bindings env

  let fold_both tp_fn tm_fn env acc =
    Id.Map.fold tm_fn env.term_ids @@ Id.Map.fold tp_fn env.term_ids acc

  let fold_type fn env acc = fold_both (fun _ _ acc -> acc) fn env acc
  let fold_term fn env acc = fold_both (fun _ _ acc -> acc) fn env acc

  let initial = of_list Input.initial_types Input.initial_terms

end
