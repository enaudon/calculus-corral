(** {1 Types} *)

(** The type of types. *)
type t

(** {1 Constructors and Destructors} *)

(** [cst id] constructs a constant with the identifier [id]. *)
val cst : string -> t

(** [var id] constructs a variable with the identifier [id]. *)
val var : string -> t

(**
  [abs arg kn body] constructs the abstraction of [arg] of kind [kn]
  from [body].
 *)
val abs : string -> Kind.t -> t -> t

(**
  [abs' args body] constructs the abstraction of [args] from [body].
 *)
val abs' : (string * Kind.t) list -> t -> t

(** [app fn arg] constructs the application of [fn] to [arg]. *)
val app : t -> t -> t

(** [app' fn arg] constructs the application of [fn] to [args]. *)
val app' : t -> t list -> t

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

(** [to_kind tp] comptutes the kind of [tp]. *)
val to_kind : t -> Kind.t

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
