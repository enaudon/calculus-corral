(** {1 Types} *)

(** The type of identifiers. *)
type t

(** {1 Exceptions} *)

(** [Unbound id] indicates that no binding exists for the [id]. *)
exception Unbound of t

(** {1 Containers} *)

(** Sets of identifiers. *)
module Set : sig

  (** The type of set elements, i.e. identifiers. *)
  type elt = t

  (** The type of sets of identifiers. *)
  type t

  (** [empty] is the empty set. *)
  val empty : t

  (** [of_list l] constructs a set from the elements in [l]. *)
  val of_list : elt list -> t

  (** [add id set] extends [set] with [id]. *)
  val add : elt -> t -> t

  (** [del id set] removes the element [id] from [set]. *)
  val del : elt -> t -> t

  (** [mem id set] determines whether [id] is a member of [set]. *)
  val mem : elt -> t -> bool

end

(** Maps from identifiers. *)
module Map : sig

  (** The type of map keys, i.e. identifiers. *)
  type key = t

  (** The type of maps from identifiers. *)
  type 'a t

  (** [empty] is the empty map. *)
  val empty : 'a t

  (** [singleton id x] constructs a map with [id] bound to [x]. *)
  val singleton : key -> 'a -> 'a t

  (** [of_list l] constructs a map from the bindings in [l]. *)
  val of_list : (key * 'a) list -> 'a t

  (** [add id x m] extends [m] with a binding from [id] to [x]. *)
  val add : key -> 'a -> 'a t -> 'a t

  (** [del id m] removes the mapping from [id] from [m]. *)
  val del : key -> 'a t -> 'a t

  (**
    [find id m] computes the value to which [id] is bound in [m].  If no
    such value exists, [find] raises [Unbound id].
   *)
  val find : key -> 'a t -> 'a

  (**
    [find_default x id m] computes the value to which [id] is bound in
    [m].  If no such value exists, [find] evaluates to [x].
   *)
  val find_default : 'a -> key -> 'a t -> 'a

  (** [bindings m] computes a list of the bindings in [m]. *)
   val bindings : 'a t -> (key * 'a) list

  (**
    [map f m] constructs a new map by applying f to each binding in [m].
   *)
   val map : ('a -> 'b) -> 'a t -> 'b t

  (**
    [fold f m init] computes [f kN vN (... (f k0 v0 init)...)], where
    the [k]'s and [v]'s are the key/value bindings in [m] in increasing
    order.
   *)
   val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

end

(** {1 Functions} *)

(** [fresh ()] produces a fresh identifier. *)
val fresh : unit -> t

(**
  [reset ()] resets the internal data structures for generating fresh
  identifiers.
 *)
val reset : unit -> unit

(** [of_string str] creates an identifier from the string [str]. *)
val of_string : string -> t

(** [to_string id] computes a string representation of [id]. *)
val to_string : t -> string

(**
  [alpha_equivalent env id1 id2] determines whether [id1] and [id2] are
  equivalent, given the renamings specified in [env].
 *)
val alpha_equivalent : (t * t) list -> t -> t -> bool
