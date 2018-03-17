(** Disjoint-Set.

  The disjoint-set data structure tracks a collection of non-overlapping
  sets of elements.  Each set has data associated with it, which known
  as the set's representative element.  The disjoint-set structure
  provides efficient methods for merging sets and finding the
  representative element of a set.
 *)

(** {1 Types} *)

(** The type of elements in a disjoint sets. *)
type 'a t

(** {1 Functions} *)

(** [singleton x] creates a set containing only [x]. *)
val singleton : 'a -> 'a t

(**
  [find x] computes the representative element of the set containing
  [x].
 *)
val find : 'a t -> 'a

(**
  [merge x y] merges the set containing [x] with the set containing [y].
  The representative element of the resulting set is the representative
  element of the set containing [x].
 *)
val merge : 'a t -> 'a t -> unit

(**
  [update x data] sets the representative element of the set containing
  [x] to [data].
 *)
val update : 'a t -> 'a -> unit
