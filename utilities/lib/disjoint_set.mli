(** Disjoint-set

    The disjoint-set data structure tracks a collection of non-overlapping sets
    of elements. Each set has data associated with it, which known as the set's
    representative element. The disjoint-set structure provides efficient
    methods for merging sets and finding the representative element of a set. *)

(** {1 Types} *)

(** The type of disjoint-set elements. *)
type elt

(** The type of disjoint-sets. *)
type 'a t

(** {1 Functions} *)

(** [empty] is the empty set. *)
val empty : 'a t

(** [singleton x ds] constructs an element containing [x] and inserts into [ds]. *)
val singleton : 'a -> 'a t -> 'a t * elt

(** [find a] computes the representative element of the set containing [a]. *)
val find : elt -> 'a t -> 'a t * 'a

(** [merge a b] merges the set containing [a] with the set containing [b]. The
    representative element of the resulting set is the representative element of
    the set containing [a]. *)
val merge : elt -> elt -> 'a t -> 'a t

(** [update a x] sets the representative element of the set containing [a] to
    [x]. *)
val update : elt -> 'a -> 'a t -> 'a t
