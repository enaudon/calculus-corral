(** {1 Types} *)

(** The type of terms. *)
type t

(** {1 Constructors} *)

(** [var id] constructs a variable with the identifier [id]. *)
val var : ?loc : Location.t -> string -> t

(** [abs args body] constructs the abstraction of [arg] from [body]. *)
val abs : ?loc : Location.t -> string -> t -> t

(**
  [abs' args body] constructs the abstraction of [args] from [body].
 *)
val abs' : ?loc : Location.t -> string list -> t -> t

(** [app fn arg] constructs the application of [fn] to [arg]. *)
val app : ?loc : Location.t -> t -> t -> t

(** [app' fn arg] constructs the application of [fn] to [args]. *)
val app' : ?loc : Location.t -> t -> t list -> t

(**
  [bind id value body] constructs the binding of [id] to [value] within
  [body].  Note that [value] is not generalized--it is monomorphic!
 *)
val bind : ?loc : Location.t -> string -> t -> t -> t

(** {1 Typing and Elaboration} *)

(**
  [to_type_hm env tm] computes the type of [tm] under [env], via
  Algorithm W-style Hindley-Milner type inference.
 *)
val to_type_hm : Type.t Identifier.Map.t -> t -> Type.t

(**
  [to_intl_repr_hm env tm] computes an internal representation term
  which is equivalent to [tm].
 *)
val to_intl_repr_hm :
  Type.t Identifier.Map.t -> t -> Universal_types.Term.t

(**
  [to_type_pr env tm] computes the type of [tm] under [env], via
  constraint-based type inference a la Pottier and Remy.
 *)
val to_type_pr : Type.t Identifier.Map.t -> t -> Type.t

(**
  [to_intl_repr_pr env tm] computes an internal representation term
  which is equivalent to [tm].
 *)
val to_intl_repr_pr :
  Type.t Identifier.Map.t -> t -> Universal_types.Term.t

(** {1 Utilities} *)

(** [to_string tm] computes a string representation of [tm]. *)
val to_string : t -> string
