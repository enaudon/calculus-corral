(** {1 Types} *)

(** The type of types. *)
type t

(** {1 Constructors and Destructors} *)

(** [base] is the base type. *)
val base : t

(** [func arg res] constructs a function from [arg] to [res]. *)
val func : t -> t -> t

(** [func' args res] constructs a function from [args] to [res]. *)
val func' : t list -> t -> t

(**
  [get_func tp] computes the argument and result type of [tp], if [tp]
  is a function.  Otherwise, [get_func] raises [Invalid_argument].
 *)
val get_func : t -> t * t

(** {1 Kinding} *)

(** [to_kind tp] computes the kind of [tp]. *)
val to_kind : t -> Kind.t

(** {1 Utilities} *)

(**
  [alpha_equivalent tp1 tp2] determines whether [tp1] and [tp2] are
  equivalent up to renaming of variables.
 *)
val alpha_equivalent : t -> t -> bool

(** [to_string tp] computes a string representation of [tp]. *)
val to_string : t -> string
