(** {1 Types} *)

(** The type of types. *)
type t

(** {1 Constructors and Destructors} *)

(** [base] is the base type. *)
val base : t

(** [var id] constructs a type variable identified by [id]. *)
val var : string -> t

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
  [alpha_equivalent ~beta_env ~env tp1 tp2] determines whether [tp1] and
  [tp2] are equivalent up to renaming of variables.  The optional
  argument, [env], specifies the renaming between bound variables, while
  [beta_env] is the beta-reduction environment.
 *)
val alpha_equivalent :
  ?beta_env : t Identifier.Map.t ->
  ?env : (Identifier.t * Identifier.t) list ->
  t ->
  t ->
  bool

(** [to_string tp] computes a string representation of [tp]. *)
val to_string : t -> string
