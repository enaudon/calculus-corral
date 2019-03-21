(** {1 Types} *)

(** The type of types. *)
type t

(** {1 Constructors and Destructors} *)

(** [var id] constructs a type variable identified by [id]. *)
val var : Identifier.t -> t

(**
  [abs arg kn body] constructs the abstraction of [arg] of kind [kn]
  from [body].
 *)
val abs : Identifier.t -> Kind.t -> t -> t

(**
  [abs' args body] constructs the abstraction of [args] from [body].
 *)
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

(**
  [get_func tp] computes the argument and result type of [tp], if [tp]
  is a function.  Otherwise, [get_func] raises [Invalid_argument].
 *)
val get_func : t -> t * t

(**
  [get_forall tp] computes the variable quantifier/kind and body of
  [tp], if [tp] is a universally quantified type.  Otherwise,
  [get_forall] raises [Invalid_argument].
 *)
val get_forall : t -> Identifier.t * Kind.t * t

(**
  [get_forall' tp] computes the variable quantifier/kind and body of
  [tp].
 *)
val get_forall' : t -> (Identifier.t * Kind.t) list * t

(** {1 Kinding} *)

(** [to_kind env tp] computes the kind of [tp] under [env]. *)
val to_kind : Kind.t Identifier.Map.t -> t -> Kind.t

(** {1 Transformations} *)

(**
  [beta_reduce ~deep:() env tp] evaluates any applications in [tp] under
  [env].  If the [deep] argument is passed, then [beta_reduce] will
  reduce the body of abstractions.
 *)
val beta_reduce : ?deep : unit -> t Identifier.Map.t -> t -> t

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

(**
  [free_vars bvs tp] computes the free variables in [tp], assuming that
  the variables in [bvs] are already bound.
 *)
val free_vars : t -> Identifier.Set.t

(**
  [subst fvars tp sub] applies the substitution [sub] to [tp], assuming
  that the identifiers in [fvars] may occur free in the range of [sub].
*)
val subst : Identifier.Set.t -> t Identifier.Map.t -> t -> t

(**
  [simplify ~ctx tp] replaces each variable in [tp] with the
  lexicographically lowest unused variable.
 *)
val simplify :
  ?ctx : (unit -> Identifier.t) * t Identifier.Map.t -> t -> t

(** [to_string tp] computes a string representation of [tp]. *)
val to_string : t -> string
