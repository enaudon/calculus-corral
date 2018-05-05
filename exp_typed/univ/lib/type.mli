(** {1 Types} *)

(** The type of types. *)
type t

(** {1 Constructors and Destructors} *)

(** [var id] constructs a type variable identified by [id]. *)
val var : string -> t

(** [func arg res] constructs a function from [arg] to [res]. *)
val func : t -> t -> t

(** [func' args res] constructs a function from [args] to [res]. *)
val func' : t list -> t -> t

(**
  [forall id tp] constructs a universally quantified type.
 *)
val forall : string -> t -> t

(**
  [forall' ids tp] constructs a universally quantified type.
 *)
val forall' : string list -> t -> t

(**
  [get_func tp] computes the argument and result type of [tp], if [tp]
  is a function.  Otherwise, [get_func] raises [Invalid_argument].
 *)
val get_func : t -> t * t

(**
  [get_forall tp] computes the variable identifier and body of [tp], if
  [tp] is a universally quantified type.  Otherwise, [get_forall] raises
  [Invalid_argument].
 *)
val get_forall : t -> Identifier.t * t

(** {1 Utilities} *)

(**
  [alpha_equivalent ~env tp1 tp2] determines whether [tp1] and [tp2] are
  equivalent up to renaming of variables.  The optional argument, [env],
  specifies the renaming between bound variables.
 *)
val alpha_equivalent :
  ?env : (Identifier.t * Identifier.t) list -> t -> t -> bool

(** [free_vars tp] computes the free variables in [tp]. *)
val free_vars : t -> Identifier.Set.t

(**
  [subst fvars tp sub] applies the substitution [sub] to [tp], assuming
  that the identifiers in [fvars] may occur free in the range of [sub].
*)
val subst : Identifier.Set.t -> t Identifier.Map.t -> t -> t

(** [to_string tp] computes a string representation of [tp]. *)
val to_string : t -> string
