module Id = Identifier

module type Input = sig
  type value

  val initial : (Id.t * value) list
end

module type Output = sig
  type value

  type t

  val empty : t

  val initial : t

  val singleton : Identifier.t -> value -> t

  val of_list : (Identifier.t * value) list -> t

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

(* TODO: Is it possible to avoid writing this explicitly? *)
module Make (Input : Input) = struct
  open Id.Map

  type t = Input.value Id.Map.t

  let empty = empty

  let initial = of_list Input.initial

  let singleton = singleton

  let of_list = of_list

  let add = add

  let del = del

  let find = find

  let find_default = find_default

  let mem = mem

  let bindings = bindings

  let keys = keys

  let values = values

  let fold = fold
end
