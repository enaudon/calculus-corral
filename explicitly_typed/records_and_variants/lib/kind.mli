(** {1 Types} *)

(** The type of kinds. *)
type t

(** {1 Constructors and Destructors} *)

(** [base] is the base kind. *)
val base : t

(** [row] is the row kind. *)
val row : t

(** [func arg res] constructs a function from [arg] to [res]. *)
val func : t -> t -> t

(** [func' args res] constructs a function from [args] to [res]. *)
val func' : t list -> t -> t

(**
  [get_func kn] computes the argument and result kind of [kn], if [kn]
  is a function.  Otherwise, [get_func] raises [Invalid_argument].
 *)
val get_func : t -> t * t

(** {1 Utilities} *)

(**
  [alpha_equivalent kn1 kn2] determines whether [kn1] and [kn2] are
  equivalent up to renaming of variables.
 *)
val alpha_equivalent : t -> t -> bool

(** [to_string kn] computes a string representation of [kn]. *)
val to_string : t -> string
