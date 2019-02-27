(** Inference variable state

  Inference variables are maintained in a rank-indexed stack of variable
  "pools."  Each time the inferencer enters the left-hand side of a
  let-expression, it pushes a new pool onto the stack to track the
  variables introduced by that let-expression.
 *)

(** The type of variable pools. *)
type t

(** [empty] is the empty set of pools. *)
val empty : t

(** [push pools] extends [pools] with an empty variable pool. *)
val push : t -> t

(** [peek pools] retrieves variable pool at the top of [pools]. *)
val peek : t -> Identifier.Set.t

(** [peek pools] removes variable pool at the top of [pools]. *)
val pop : t -> t

(**
  [register pools id] inserts [id] into the variable pool at the top of
  [pools].
 *)
val register : t -> Identifier.t -> t

(** [unregister pools id] removes [id] from [pools]. *)
val unregister : t -> Identifier.t -> t

(**
  [update pools id1 id2] moves [id1] to the variable pool of which [id2]
  is a member, if [id2]'s rank is lower than [id1]'s.
 *)
val update : t -> Identifier.t -> Identifier.t -> t

(**
  [is_mono pools id] evaluates to [true] if [id] has a monomorphic
  rank, or [false] otherwise.
 *)
val is_mono : t -> Identifier.t -> bool
