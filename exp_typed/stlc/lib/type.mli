(** {1 Types} *)

(** The type of types. *)
type t

(** {1 Exceptions} *)

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

(** {1 Utilities} *)

(**
  [struct_equivalent tp1 tp2] evaluates to [true] if [tp1] and [tp2] are
  structurally equivalent to one another.
 *)
val struct_equivalent : t -> t -> bool

(**
  [alpha_equivalent tp1 tp2] evaluates to [true] if [tp1] and [tp2] are
  equal upto renaming of bound variables, or [false] otherwise.
 *)
val alpha_equivalent : t -> t -> bool

(** [to_string tp] computes a string representation of [tp]. *)
val to_string : t -> string
