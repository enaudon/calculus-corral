(** {1 Types} *)

(** The type of types. *)
type t

(** {1 Exceptions} *)

(**
  [Occurs (id, tp)] indicates that unification failed because the type
  variable identified by [id] occurs in the type [tp].
 *)
exception Occurs of Identifier.t * t

(**
  [Cannot_unify (tp1, tp2)] indicates that unification failed because
  [tp1] and [tp2] could not be unified.
 *)
exception Cannot_unify of t * t

(** {1 Constructors and Destructors} *)

(** [var id] constructs a variable with the identifier [id]. *)
val var : Identifier.t -> t

(**
  [func arg res] constructs a function from [arg] to [res].  If either
  [arg] or [res] is polymoprhic, [func] will raise [Expected_mono].
 *)
val func : t -> t -> t

(**
  [func' args res] constructs a function from [args] to [res].  If
  either [args] or [res] is polymoprhic, [func'] will raise
  [Expected_mono].
 *)
val func' : t list -> t -> t

(** [rcrd fields] constructs a record type. *)
val rcrd : (Identifier.t * t) list -> t option -> t

(** [vnrt cases] constructs a variant type. *)
val vrnt : (Identifier.t * t) list -> t option -> t

(** [get_forall' tp] computes the variable quantifier of [tp]. *)
val get_quants : t -> (Identifier.t * Kind.t) list

(** {1 Kinding} *)

(** [default_env] is the default typing environment. *)
val default_env : Kind.t Identifier.Map.t

(** [to_kind env tp] computes the kind of [tp] under [env]. *)
val to_kind : Kind.t Identifier.Map.t -> t -> Kind.t

(** {1 Inference} *)

(** Inference State

  This module encapsulates the internal state of the inference engine.
  The most significant piece of the state, from the user's perspective,
  is a substitution, which represents the solution computed by type
  inference.  After type inference this solution may be applied via the
  [apply_solution] function.
 *)
module State : sig

  (** The type of inference engine state. *)
  type s

  (** [initial] is the initial state. *)
  val initial : s

  (**
    [apply_solution tp state] applies the solution in [state] to [tp],
    replacing any variables in [tp] which occur in the domain of the
    substitution with their corresponding concrete types in the range of
    the substitution.
   *)
  val apply_solution : t -> s -> t

end

(**
  [unify sub tp1 tp2] computes the subtitution which unifies [tp1] and
  [tp2].
 *)
val unify : State.s -> t -> t -> State.s

(**
  [register state tv kn] registers the type variable, [tv], of kind
  [kn], with the inference engine so that it may be used in
  type-checking.
 *)
val register : State.s -> t -> Kind.t -> State.s

(**
  [gen_enter state] updates the state before type-checking the left-hand
  side of a let-expression.
 *)
val gen_enter : State.s -> State.s

(**
  [gen_exit state tp] updates the internal state and performs
  generalization after type-checking the left-hand side of a
  let-expression.  Here, generalization involves replacing all
  monomorphic variables introduced within the let-expression with
  polymorphic variables.  The result contains three things: a) the
  updated state, b) a list of identifiers corresponding the newly
  polymorphic variables and their kinds, and c) the newly-polymorphic
  type.
 *)
val gen_exit : State.s -> t -> State.s * Kind.t Identifier.Map.t * t

(**
  [inst state tp] replaces all polymorphic variables in [tp] with fresh
  monomorphic variables.  The result contains three things: a) the
  updated state, b) a list of the fresh monomorphic variables, and c)
  the newly-monomorphic type.
 *)
val inst : State.s -> t -> State.s * t list * t

(** {1 Utilities} *)

(**
  [to_intl_repr tp] computes an internal representation type which is
  equivalent to [tp].
 *)
val to_intl_repr : t -> Records_and_variants.Type.t

(**
  [simplify tp] replaces each variable in [tp] with the
  lexicographically lowest unused variable.

  NOTE: [simplify]'d types cannot be used with inference functions.
 *)
val simplify : t -> t

(**
  [to_string ~no_simp:() ~show_quants:() tp] computes a string
  representation of [tp].  Unless [no_simp] is passed, [tp] is
  simplified first.  Unless [show_quants] is passed, the universal
  quantifiers are not printed.
 *)
val to_string : ?no_simp : unit -> ?show_quants : unit -> t -> string
