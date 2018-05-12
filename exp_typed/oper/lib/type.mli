(** {1 Types} *)

(** The type of types. *)
type t

(** {1 Constructors and Destructors} *)

(** [base] is the base type. *)
val base : t

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

(** [default_env] is the default typing environment. *)
val default_env : Kind.t Identifier.Map.t

(** [to_kind tp] computes the kind of [tp] under [env]. *)
val to_kind : ?env : Kind.t Identifier.Map.t -> t -> Kind.t

(** {1 Transformations} *)

(**
  [beta_reduce ~deep tp] reduces any applications in [tp] under [env].
  If the [deep] argument is passed, then [beta_reduce] will reduce the
  body of abstractions.
 *)
val beta_reduce : ?deep : unit -> ?env : t Identifier.Map.t -> t -> t

(** {1 Utilities} *)

(**
  [alpha_equivalent tp1 tp2] determines whether [tp1] and [tp2] are
  equivalent up to renaming of variables.
 *)
val alpha_equivalent : t -> t -> bool

(** [to_string tp] computes a string representation of [tp]. *)
val to_string : t -> string
