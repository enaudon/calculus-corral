(** {1 Types} *)

(** The type of terms. *)
type t

(** {1 Constructors} *)

(** [var id] constructs a variable with the identifier [id]. *)
val var : ?loc : Location.t -> Identifier.t -> t

(** [abs args body] constructs the abstraction of [arg] from [body]. *)
val abs : ?loc : Location.t -> Identifier.t -> t -> t

(**
  [abs' args body] constructs the abstraction of [args] from [body].
 *)
val abs' : ?loc : Location.t -> Identifier.t list -> t -> t

(** [app fn arg] constructs the application of [fn] to [arg]. *)
val app : ?loc : Location.t -> t -> t -> t

(** [app' fn arg] constructs the application of [fn] to [args]. *)
val app' : ?loc : Location.t -> t -> t list -> t

(**
  [bind id value body] constructs the binding of [id] to [value] within
  [body].
 *)
val bind : ?loc : Location.t -> Identifier.t -> t -> t -> t

(**
  [annot tm annot] constructs an annotation, which assigns [tm] the type
  given by [annot].
 *)
val annot : ?loc : Location.t -> t -> Type_annotation.t -> t

(** {1 Typing and Elaboration} *)

(**
  [to_type_hm env tm] computes the type of [tm] under [env], via
  Algorithm W-style Hindley-Milner type inference.
 *)
val to_type_hm :
  (Kind.Environment.t * Type.Environment.t) -> t -> Type.t

(**
  [to_intl_repr_hm env tm] computes an internal representation term
  which is equivalent to [tm].
 *)
val to_intl_repr_hm :
  (Kind.Environment.t * Type.Environment.t) ->
  t ->
  Type_operators_exp.Term.t

(**
  [to_type_pr env tm] computes the type of [tm] under [env], via
  constraint-based type inference a la Pottier and Remy.
 *)
val to_type_pr :
  (Kind.Environment.t * Type.Environment.t) -> t -> Type.t

(**
  [to_intl_repr_pr env tm] computes an internal representation term
  which is equivalent to [tm].
 *)
val to_intl_repr_pr :
  (Kind.Environment.t * Type.Environment.t) ->
  t ->
  Type_operators_exp.Term.t

(** {1 Utilities} *)

(** [to_string tm] computes a string representation of [tm]. *)
val to_string : t -> string
