(** {1 Types} *)

(** The type of type constraints. *)
type 'a t

(** {1 Constructors} *)

(**
  [inst id tp] ensures that [tp] is an instance of the scheme to
  which [id] is bound.
 *)
val inst :
  ?loc : Location.t ->
  Identifier.t ->
  Type.t ->
  Universal_types.Type.t list t

(** [equals lhs rhs] equates the two types [lhs] and [rhs]. *)
val equals : ?loc : Location.t -> Type.t -> Type.t -> unit t

(** [conj lhs rhs] conjoins of the constraints [lhs] and [rhs]. *)
val conj : ?loc : Location.t -> 'a t -> 'b t -> ('a * 'b) t

(** [exists rank (fun tp -> c)] existentially quantifies [tp] in [c]. *)
val exists :
  ?loc : Location.t ->
  int ->
  (Type.t -> 'a t) ->
  (Universal_types.Type.t * 'a) t

(** [def id tp c] binds [id] to [tp] within [c]. *)
val def : ?loc : Location.t -> Identifier.t -> Type.t -> 'a t -> 'a t

(**
  [let_ rank id (fun tp -> c1) c2] binds [id] to [tp] within [c2], given
  that [c1] is satisfiable.
 *)
val let_ :
  ?loc : Location.t ->
  int ->
  Identifier.t ->
  (Type.t -> 'a t) ->
  'b t ->
  (Universal_types.Type.t * 'a * 'b) t

(**
  [map f c] produces a constraint that is identical to [c] except that
  it produces a different value.  If [c] produces [v], then [map f c]
  produces [f v].
 *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** {1 Solving} *)

(**
  [solve rank c] solves [c] and, if [c] is satisfiable, produces a
  result value.
 *)
val solve : int -> 'a t -> 'a

(** {1 Utilities} *)

(**
  [to_string tp] computes a string representation of [c].  Unless
  [no_simp] is passed, the types in [c] are simplified first.
 *)
val to_string : ?no_simp : unit -> 'a t -> string

module Operators : sig

  (** [c <$> f] is an infix shorthand for [map f c]. *)
  val ( <$> ) : 'a t -> ('a -> 'b) -> 'b t

end
