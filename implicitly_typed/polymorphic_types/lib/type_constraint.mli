(** {1 Types} *)

(** The type of type constraints. *)
type t

(** {1 Constructors} *)

(**
  [inst rank id tp] ensures that [tp] is an instance of the scheme to
  which [id] is bound.
 *)
val inst : ?loc : Location.t -> int -> Identifier.t -> Type.t -> t

(** [equals lhs rhs] equates the two types [lhs] and [rhs]. *)
val equals : ?loc : Location.t -> Type.t -> Type.t -> t

(** [conj lhs rhs] conjoins of the constraints [lhs] and [rhs]. *)
val conj : ?loc : Location.t -> t -> t -> t

(** [exists (fun id -> c)] existentially quantifies [id] in [c]. *)
val exists : ?loc : Location.t -> (Identifier.t -> t) -> t

(** [def id tp c] binds [id] to [tp] within [c]. *)
val def : ?loc : Location.t -> Identifier.t -> Type.t -> t -> t

(** {1 Solving} *)

(** [solve c] solves [c]. *)
val solve : t -> unit

(** {1 Utilities} *)

(**
  [to_string tp] computes a string representation of [c].  Unless
  [no_simp] is passed, the types in [c] are simplified first.
 *)
val to_string : ?no_simp : unit -> t -> string
