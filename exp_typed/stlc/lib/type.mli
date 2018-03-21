(** {1 Types} *)

(** The type of types. *)
type t

(** {1 Exceptions} *)

(** {1 Constructors and Destructors} *)

(** [base name] constructs a base type identified by [name]. *)
val base : string -> t

(** [func arg res] constructs a function from [arg] to [res]. *)
val func : t -> t -> t

(** [func' args res] constructs a function from [args] to [res]. *)
val func' : t list -> t -> t

(**
  [get_func tp] computes the argument and result type of [tp], if [tp]
  is a function.  Otherwise, [get_func] raises [Invalid_argument].
 *)
val get_func : t -> t * t

(** {1 Utilities} *)

(**
  [equals tp1 tp2] evaluates to [true] if [tp1] and [tp2] are equal, or
  [false] otherwise.
 *)
val equals : t -> t -> bool

(** [to_string tp] computes a string representation of [tp]. *)
val to_string : t -> string
