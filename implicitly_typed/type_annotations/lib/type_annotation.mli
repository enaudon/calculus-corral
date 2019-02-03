(** The type of type annotation. *)
type t

(** [typo tp] constructs an annotation from [tp]. *)
val typo : Type.t -> t

(**
  [forall ids tp] constructs a universally quantified type annotation.
 *)
val forall : Identifier.t list -> Type.t -> t

(**
  [exists ids tp] constructs a existentially quantified type annotation.
 *)
val exists : Identifier.t list -> Type.t -> t

(**
  [get_forall annot] computes the universal quantifiers and type of
  [annot].
 *)
val get_forall : t -> Identifier.t list * Type.t

(**
  [get_exists annot] computes the existential quantifiers and type of
  [annot].
 *)
val get_exists : t -> Identifier.t list * Type.t

(**
  [get_typo annot] computes the type making up the body of [annot].
 *)
val get_typo : t -> Type.t

(** TODO: Comment. *)
val infer : Type.Inferencer.state -> t -> Type.Inferencer.state * Type.t

(**
  [constrain annot term_co_fn] constructs a constraint which ensures
  that [annot] holds.
 *)
val constrain :
  t ->
  (Type.t -> 'a Type_constraint.t * 'b Type_constraint.t) ->
  'a Type_constraint.t

(** [to_string annot] computes a string representation of [annot]. *)
val to_string : t -> string
