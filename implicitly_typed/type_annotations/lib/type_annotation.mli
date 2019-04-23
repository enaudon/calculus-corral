(** The type of type annotation. *)
type t

(** [typo tp] constructs an annotation from [tp]. *)
val typo : Type.t -> t

(**
  [forall id kn tp] constructs a universally quantified type annotation.
 *)
val forall : Identifier.t -> Kind.t -> t -> t

(**
  [exists id kn tp] constructs a existentially quantified type annotation.
 *)
val exists : Identifier.t -> Kind.t -> t -> t

(** TODO: Comment. *)
val infer :
  Type.Environment.t ->
  Type.Inferencer.state ->
  t ->
  Type.Inferencer.state * Type.t

(**
  [constrain annot term_co_fn] constructs a constraint which ensures
  that [annot] holds.
 *)
val constrain :
  Type.Environment.t ->
  t ->
  (Type.t -> 'a Type_constraint.t * 'b Type_constraint.t) ->
  'a Type_constraint.t

(** [to_string annot] computes a string representation of [annot]. *)
val to_string : t -> string
