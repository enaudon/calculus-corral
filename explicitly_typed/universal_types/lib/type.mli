(** {1 Types} *)

(** The type of types. *)
type t

(** {1 Containers} *)

(** Environment of bound types. *)
module Environment : Type_environment.Output with type value := t

(** {1 Constructors and Destructors} *)

(** [var id] constructs a type variable identified by [id]. *)
val var : Identifier.t -> t

(** [func arg res] constructs a function from [arg] to [res]. *)
val func : t -> t -> t

(** [func' args res] constructs a function from [args] to [res]. *)
val func' : t list -> t -> t

(** [forall id tp] constructs a universally quantified type. *)
val forall : Identifier.t -> t -> t

(** [forall' ids tp] constructs a universally quantified type. *)
val forall' : Identifier.t list -> t -> t

(** [get_func tp] computes the argument and result type of [tp], if [tp] is a
    function. Otherwise, [get_func] raises [Invalid_argument]. *)
val get_func : t -> t * t

(** [get_forall tp] computes the variable quantifier and body of [tp], if [tp]
    is a universally quantified type. Otherwise, [get_forall] raises
    [Invalid_argument]. *)
val get_forall : t -> Identifier.t * t

(** [get_forall' tp] computes the variable quantifier and body of [tp]. *)
val get_forall' : t -> Identifier.t list * t

(** {1 Transformations} *)

(** [reduce_one env tp] evaluates top-level applications and unrolls top-level
    recursive types in [tp] under [env]. *)
val reduce_one : Environment.t -> t -> t

(** {1 Utilities} *)

(** [check env tp] verifies that [tp] is well-formed under [env]. In the absence
    of kinding, this just means checking that all variables are bound. *)
val check : Identifier.Set.t -> t -> unit

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
