(** {1 Types} *)

(** The type of terms. *)
type t

(** {1 Constructors} *)

(** [var id] constructs a variable with the identifier [id]. *)
val var : ?loc : Location.t -> string -> t

(**
  [abs arg tp body] constructs the abstraction of [arg] of type [tp]
  from [body].
 *)
val abs : ?loc : Location.t -> string -> Type.t -> t -> t

(**
  [abs' args body] constructs the abstraction of [args] from [body].
 *)
val abs' : ?loc : Location.t -> (string * Type.t) list -> t -> t

(** [app fn arg] constructs the application of [fn] to [arg]. *)
val app : ?loc : Location.t -> t -> t -> t

(** [app' fn arg] constructs the application of [fn] to [args]. *)
val app' : ?loc : Location.t -> t -> t list -> t

(** {1 Typing} *)

(** [to_type ~env tm] computes the type of [tm] under [env]. *)
val to_type : ?env : Type.t Identifier.Map.t -> t -> Type.t

(** {1 Transformations} *)

(**
  [beta_reduce tm] evaluates any applications in [tm] under [env]. If
  the [deep] argument is passed, then [beta_reduce] will evaluate the
  body of abstractions.
 *)
val beta_reduce : ?deep : unit -> ?env : t Identifier.Map.t -> t -> t

(** {1 Utilities} *)

(**
  [alpha_equivalent tm1 tm2] evaluates to [true] if [tm1] and [tm2] are
  equal up renaming of bound variables, or [false] otherwise.
 *)
val alpha_equivalent : t -> t -> bool

(** [to_string tm] computes a string representation of [tm]. *)
val to_string : t -> string
