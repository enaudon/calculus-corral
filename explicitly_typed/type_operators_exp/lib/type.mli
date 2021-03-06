(** {1 Types} *)

(** The type of types. *)
type t

(** {1 Containers} *)

(** Environment of bound types. *)
module Environment : Type_environment.Output with type value := t

(** {1 Constructors and Destructors} *)

(** [var id] constructs a type variable identified by [id]. *)
val var : Identifier.t -> t

(** [abs arg kn body] constructs the abstraction of [arg] of kind [kn] from
    [body]. *)
val abs : Identifier.t -> Kind.t -> t -> t

(** [abs' args body] constructs the abstraction of [args] from [body]. *)
val abs' : (Identifier.t * Kind.t) list -> t -> t

(** [app fn arg] constructs the application of [fn] to [arg]. *)
val app : t -> t -> t

(** [app' fn arg] constructs the application of [fn] to [args]. *)
val app' : t -> t list -> t

(** [func arg res] constructs a function from [arg] to [res]. *)
val func : t -> t -> t

(** [func' args res] constructs a function from [args] to [res]. *)
val func' : t list -> t -> t

(** [forall id kn tp] constructs a universally quantified type. *)
val forall : Identifier.t -> Kind.t -> t -> t

(** [forall' ids tp] constructs a universally quantified type. *)
val forall' : (Identifier.t * Kind.t) list -> t -> t

(** [get_func tp] computes the argument and result type of [tp], if [tp] is a
    function. Otherwise, [get_func] raises [Invalid_argument]. *)
val get_func : t -> t * t

(** [get_forall tp] computes the variable quantifier/kind and body of [tp], if
    [tp] is a universally quantified type. Otherwise, [get_forall] raises
    [Invalid_argument]. *)
val get_forall : t -> Identifier.t * Kind.t * t

(** [get_forall' tp] computes the variable quantifier/kind and body of [tp]. *)
val get_forall' : t -> (Identifier.t * Kind.t) list * t

(** {1 Kinding} *)

(** [to_kind env tp] computes the kind of [tp] under [env]. *)
val to_kind : Kind.Environment.t -> t -> Kind.t

(** {1 Transformations} *)

(** [reduce env tp] resolves top-level variables and evaluates top-level
    applications in [tp]. *)
val reduce : Environment.t -> t -> t

(** {1 Utilities} *)

(** [alpha_equivalent ~beta_env ~env tp1 tp2] determines whether [tp1] and [tp2]
    are equivalent up to renaming of variables. The optional argument, [env],
    specifies the renaming between bound variables, while [beta_env] is the
    beta-reduction environment. *)
val alpha_equivalent :
  ?beta_env:Environment.t ->
  ?env:(Identifier.t * Identifier.t) list ->
  t ->
  t ->
  bool

(** [subst fvs sub tp] applies [sub] to [tp], replacing any variable in the
    domain of [sub] with the corresponding type the range of [sub]. [fvs] is any
    superset of the variables which appear in the range of [sub]. *)
val subst : Identifier.Set.t -> Environment.t -> t -> t

(** [simplify ~ctx tp] replaces each variable in [tp] with the lexicographically
    lowest unused variable. *)
val simplify : ?ctx:(unit -> Identifier.t) * t Identifier.Map.t -> t -> t

(** [to_string tp] computes a string representation of [tp]. *)
val to_string : t -> string
