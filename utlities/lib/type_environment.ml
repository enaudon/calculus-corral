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

  module Type : sig
    val singleton : Identifier.t -> value -> t
    val add : Identifier.t -> value -> t -> t
    val del : Identifier.t -> t -> t
    val find : Identifier.t -> t -> value
    val find_default : value -> Identifier.t -> t -> value
    val mem : Identifier.t -> t -> bool
    val bindings : t -> (Identifier.t * value) list
    val keys : t -> Identifier.t list
    val values : t -> value list
    val fold : (Identifier.t -> value -> 'a -> 'a) -> t -> 'a -> 'a
  end

  module Term : sig
    val singleton : Identifier.t -> value -> t
    val add : Identifier.t -> value -> t -> t
    val del : Identifier.t -> t -> t
    val find : Identifier.t -> t -> value
    val find_default : value -> Identifier.t -> t -> value
    val mem : Identifier.t -> t -> bool
    val bindings : t -> (Identifier.t * value) list
    val keys : t -> Identifier.t list
    val values : t -> value list
    val fold : (Identifier.t -> value -> 'a -> 'a) -> t -> 'a -> 'a
  end

  val of_list :
    (Identifier.t * value) list -> (Identifier.t * value) list -> t
  val bindings : t -> (Identifier.t * value) list
  val fold :
    (Identifier.t -> value -> 'a -> 'a) ->
    (Identifier.t -> value -> 'a -> 'a) ->
    t ->
    'a ->
    'a

end

module Make (Input : Input) = struct

  open Id.Map

  type nonrec t = {
    type_ids : Input.value t;
    term_ids : Input.value t;
  }

  let empty = {type_ids = empty; term_ids = empty}

  module Type = struct
    let lift_fn fn env = {env with type_ids = fn env.type_ids}
    let add id v = lift_fn @@ add id v
    let singleton id tp = add id tp empty
    let del id = lift_fn @@ del id
    let find id env = find id env.type_ids
    let find_default v id env = find_default v id env.type_ids
    let mem id env = mem id env.type_ids
    let bindings env = bindings env.type_ids
    let keys env = keys env.type_ids
    let values env = values env.type_ids
    let fold fn env acc = fold fn env.type_ids acc
  end

  module Term = struct
    let lift_fn fn env = {env with term_ids = fn env.term_ids}
    let add id v = lift_fn @@ add id v
    let singleton id tp = add id tp empty
    let del id = lift_fn @@ del id
    let find id env = find id env.term_ids
    let find_default v id env = find_default v id env.term_ids
    let mem id env = mem id env.term_ids
    let bindings env = bindings env.term_ids
    let keys env = keys env.term_ids
    let values env = values env.term_ids
    let fold fn env acc = fold fn env.term_ids acc
  end

  let of_list tps tms =
    empty
      |> List.fold_right (fun (id, v) -> Type.add id v) tps
      |> List.fold_right (fun (id, v) -> Term.add id v) tms

  let bindings env = Type.bindings env @ Term.bindings env

  let fold tp_fn tm_fn env acc =
    Term.fold tm_fn env @@ Type.fold tp_fn env acc

  let initial = of_list Input.initial_types Input.initial_terms

end
