(** Inference variable environment

    The inference variables environment is organized into a rank-indexed stack
    of "pools." Each time the inferencer enters the left-hand side of a
    let-expression, it pushes a new pool onto the stack to track the variables
    introduced by that let-expression. *)

(** The type of variable pool stacks. *)
type 'a t

(** [empty] is the empty stack of pools. *)
val empty : 'a t

(** [push pools] extends [pools] with an empty variable pool. *)
val push : 'a t -> 'a t

(** [peek pools] retrieves variable pool at the top of [pools]. *)
val peek : 'a t -> 'a Identifier.Map.t

(** [peek pools] removes variable pool at the top of [pools]. *)
val pop : 'a t -> 'a t

(** [insert id data pools] associates [data] with [id] in the variable pool at
    the top of [pools]. *)
val insert : Identifier.t -> 'a -> 'a t -> 'a t

(** [remove id pools] deletes [id] from [pools]. *)
val remove : Identifier.t -> 'a t -> 'a t

(** [find id pools] retrieves the data associated with [id] in [pools]. *)
val find : Identifier.t -> 'a t -> 'a

(** [update id1 id2 pools] moves [id1] to the variable pool of which [id2] is a
    member, if [id2]'s rank is lower than [id1]'s. *)
val update : Identifier.t -> Identifier.t -> 'a t -> 'a t
