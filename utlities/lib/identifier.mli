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

  (** [add id s] extends [s] with [id]. *)
  val add : elt -> t -> t

  (** [del id s] removes the element [id] from [s]. *)
  val del : elt -> t -> t

  (** [mem id s] determines whether [id] is a member of [s]. *)
  val mem : elt -> t -> bool

  (** [elements s] computes a list of the elements in [s]. *)
  val elements : t -> elt list

  (**
    [iter fn s] applies [fn] to each element in [s] in increasing order.
   *)
  val iter : (elt -> unit) -> t -> unit

  (**
    [partition fn s] computes [(s1,s2)], where [s1] is the subset of
    [s] which satisfies the predicate, [fn], and [s2] is the subset
    which does not.
   *)
  val partition : (elt -> bool) -> t -> t * t

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

  (** [mem id m] determines whether [id] is a member of [m]. *)
  val mem : key -> 'a t -> bool

  (** [bindings m] computes a list of the bindings in [m]. *)
  val bindings : 'a t -> (key * 'a) list

  (** [keys m] computes a list of the keys in [m]. *)
  val keys : 'a t -> key list

  (** [values m] computes a list of the values in [m]. *)
  val values : 'a t -> 'a list

  (**
    [map fn m] constructs a new map by applying [fn] to each binding in
    [m] in increasing order.
   *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (**
    [fold fn m init] computes [fn kN vN (... (fn k0 v0 init)...)], where
    the [k]'s and [v]'s are the key/value bindings in [m] in increasing
    order.
   *)
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

end

(** {1 Functions} *)

(**
  [define str] constructs a user-defined identifier with the specified
  string [str].
 *)
val define : string -> t

(**
  [gen_lower str] constructs a generated identifier with a fresh
  lowercase string.
 *)
val gen_lower : unit -> t

(**
  [gen_upper str] constructs a generated identifier with a fresh
  upper string.
 *)
val gen_upper : unit -> t

(**
  [is_defined id] returns [true] if [id] was user-defined, or [false]
  otherwise.
 *)
val is_defined : t -> bool

(**
  [is_generated id] returns [true] if [id] was generated, or [false]
  otherwise.
 *)
val is_generated : t -> bool

(**
  [reset ()] resets the internal data structures for generating fresh
  identifiers.
 *)
val reset : unit -> unit

(** [to_string id] computes a string representation of [id]. *)
val to_string : t -> string

(**
  [alpha_equivalent env id1 id2] determines whether [id1] and [id2] are
  equivalent, given the renamings specified in [env].
 *)
val alpha_equivalent : (t * t) list -> t -> t -> bool
