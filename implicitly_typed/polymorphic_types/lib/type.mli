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
  the types [tp1] and [tp2] are not unifiable.
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

(** {1 Inference} *)

(** Substitution

  A substitution maps (type variable) identifiers to types, and provide
  operations for extending substitutions with new mappings and for
  applying substitutions to types.  Furthermore, substitutions are
  idempotent by construction.
 *)
module Substitution : sig

  (** The type of substitutions. *)
  type s

  (**
    [identity] is the identity substitution.  It maps every variable
    to itself.
  *)
  val identity : s

  (**
    [extend id tp sub] extends [sub] with a mapping from [id] to [tp].
   *)
  val extend : Identifier.t -> t -> s -> s

  (**
    [apply tp sub] applies [sub] to [tp], replacing any variables in
    [tp] which occur in the domain of [sub] with their corresponding
    types in the range of [sub].
   *)
  val apply : t -> s -> t

end

(**
  [unify sub tp1 tp2] computes the subtitution which unifies [tp1] and
  [tp2].
 *)
val unify : Substitution.s -> t -> t -> Substitution.s

val register : t -> unit

(**
  [gen_enter ()] updates the internal state before type-checking the
  left-hand side of a let-expression.
 *)
val gen_enter : unit -> unit

(**
  [gen_exit tp] updates the internal state and performs generalization
  after type-checking the left-hand side of a let-expression.  Here,
  generalization involves replacing all monomorphic variables introduced
  within the let-expression with polymorphic variables.  The result is a
  list of identifiers corresponding the newly polymorphic variables.
 *)
val gen_exit : t -> Identifier.Set.t * Identifier.t list

(**
  [inst tp] replaces all polymorphic variables in [tp] with fresh
  monomorphic variables.  The result is a pair containing the
  monomorphized type, along with a list of the fresh monomorphic
  variables.
 *)
val inst : t -> t list * t

(** {1 Utilities} *)

(**
  [to_intl_repr tp] computes an internal representation type which is
  equivalent to [tp].
 *)
val to_intl_repr : t -> Universal_types.Type.t

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
