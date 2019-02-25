(** Stack

  The stack data structure implements a last-in, first out collection of
  elements.
 *)

(* The type of stacks. *)
type 'a t

(** [empty] is the empty stack. *)
val empty : 'a t

(** [size s] computes the number of elements in [s]. *)
val size : 'a t -> int

(** [push s x] adds [x] to the top of [s]. *)
val push : 'a -> 'a t -> 'a t

(** [peek s] retrieves the top element of [s]. *)
val peek : 'a t -> 'a

(** [pop s] removes the top element of [s]. *)
val pop : 'a t -> 'a t

(**
  [get i s] retrieves the [i]th element from [s].  The first element
  pushed onto [s] is at index [0], while the last element is at index
  [size s - 1].
 *)
val get : int -> 'a t -> 'a

(**
  [update i x s] updates the [i]th element from [s] with [x].  The first
  element pushed onto [s] is at index [0], while the last element is at
  index [size s - 1].
 *)
val update : int -> 'a -> 'a t -> 'a t
