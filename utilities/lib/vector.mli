(** Vector

    The vector data structure implements arrays of variable size. Like arrays,
    vectors are a random-access data structure and their elements are identified
    by integer indices. *)

(** {1 Types} *)

(** The types of vectors containing elements of type ['a]. *)
type 'a t

(** {1 Functions} *)

(** [empty] is the empty vector. *)
val empty : 'a t

(** [size v] computes the number of elements in [v]. *)
val size : 'a t -> int

(** {2 Array Interface} *)

(** [set i x v] sets the [i]th element in [v] to [x]. *)
val set : int -> 'a -> 'a t -> 'a t

(** [get i v] gets the [i]th element in [v]. *)
val get : int -> 'a t -> 'a

(** {2 Stack Interface} *)

(** [push_back v x] adds [x] to the end of [v], incrementing [v]'s size. *)
val push_back : 'a -> 'a t -> 'a t

(** [peek_back v] retrieves the element at the end of [v]. *)
val peek_back : 'a t -> 'a

(** [pop_back v] removes the element at the end [v], decrementing [v]'s size. *)
val pop_back : 'a t -> 'a t
