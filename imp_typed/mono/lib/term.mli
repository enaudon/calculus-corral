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

(** {1 Typing} *)

(**
  [to_type_hm tm] comptutes the type of [tm], via Algorithm W-style
  Hindley-Milner type inference.
 *)
val to_type_hm : t -> Type.t

(**
  [to_type_pr tm] comptutes the type of [tm], via constraint-based type
  inference a la Pottier and Remy.
 *)
val to_type_pr : t -> Type.t

(** {1 Utilities} *)

(** [to_string tm] computes a string representation of [tm]. *)
val to_string : t -> string
