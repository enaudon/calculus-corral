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

(** [get_forall' tp] computes the variable quantifier of [tp]. *)
val get_quants : t -> (Identifier.t * Kind.t) list

(** Inferencer

  This module contains kinding-checking functionality, as well as type
  inference functionality, like unification, generalization and
  instantiation.  After inference is complete, the [apply] function will
  apply the substitution computed by type inference.
 *)
module Inferencer : sig

  (** The type of inference engine state. *)
  type state

  (** [initial] is the initial state. *)
  val initial : state

  (**
    [register ~rigid:() state tv] registers the type variable, [tv],
    with the inference engine.  Unless [rigid] is passed, [tv] will be
    registered as a flexible type variable.
   *)
  val register : ?rigid : unit -> state -> t -> Kind.t -> state

  (** {2 Kinding} *)

  (** [default_env] is the default typing environment. *)
  val default_env : Kind.t Identifier.Map.t

  (** [to_kind state tp] computes the kind of [tp]. *)
  val to_kind : state -> t -> Kind.t

  (** {2 Typing} *)

  (**
    [unify sub tp1 tp2] computes the subtitution which unifies [tp1] and
    [tp2].  In cases where both [tp1] and [tp2] are variables, and either
    identifier may be kept in the substitution, [tp1]'s identifier is
    kept.
   *)
  val unify : state -> t -> t -> state

  (**
    [gen_enter state] updates the state before type-checking the left-hand
    side of a let-expression.
   *)
  val gen_enter : state -> state

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
  val gen_exit : state -> t -> state * Kind.t Identifier.Map.t * t

  (**
    [inst state tp] replaces all polymorphic variables in [tp] with fresh
    monomorphic variables.  The result contains three things: a) the
    updated state, b) a list of the fresh monomorphic variables, and c)
    the newly-monomorphic type.
   *)
  val inst : state -> t -> state * t list * t

  (**
    [apply tp state] applies the substitution in [state] to [tp],
    replacing any variables in [tp] which occur in the domain of the
    substitution with their corresponding concrete types in the range of
    the substitution.
   *)
  val apply : state -> t -> t

end

(** {1 Utilities} *)

(**
  [to_intl_repr tp] computes an internal representation type which is
  equivalent to [tp].
 *)
val to_intl_repr : t -> Type_operators.Type.t

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
