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

(** [to_type tm] comptutes the type of [tm]. *)
val to_type : t -> Type.t

(** {1 Utilities} *)

(** [to_string tm] computes a string representation of [tm]. *)
val to_string : t -> string
