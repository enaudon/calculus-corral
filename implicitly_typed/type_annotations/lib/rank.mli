(** Rank

  A rank is an abstract type with a total ordering, which is used to
  represent the nesting-depth of the nearest let-expression a type
  variable.
 *)

(** {1 Types} *)

(** The type of ranks. *)
type t

(** {1 Functions} *)

(** [init] is the initial rank. *)
val init : t

(** {1 Containers} *)

(** Pools of rank-indexed type variables. *)
module Pools : sig

  (** The type of variable pools. *)
  type p

  (** [empty] is the empty set of pools. *)
  val empty : p

  (**
    [push pools] extends [pools] with an empty set of variables
    associated with the rank following current top rank.
   *)
  val push : p -> p

  (**
    [peek pools] computes the variables associated with the current top
    rank in [pools].
   *)
  val peek : p -> Kind.t Identifier.Map.t

  (**
    [pop pools] removes the variables associated with the current top
    rank from [pools].
   *)
  val pop : p -> p

  (**
    [register pools id] inserts [id] into the set of variables in
    [pools] associated with the current top rank.
   *)
  val register : p -> Identifier.t -> Kind.t -> p

  (** [unregister pools id] removes [id] from [pools]. *)
  val unregister : p -> Identifier.t -> p

  (**
    [update pool id1 id2] moves [id1] to the set of variables in [pools]
    of which [id2] is a member, if [id2]'s rank is lower than [id1]'s.
   *)
  val update : p -> Identifier.t -> Identifier.t -> p

  (**
    [is_mono pools -> id] evaluates to [true] if [id] has a monomorphic
    rank, or [false] otherwise.
   *)
  val is_mono : p -> Identifier.t -> bool

end
