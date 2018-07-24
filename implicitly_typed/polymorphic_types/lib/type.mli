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

(**
  [Expected_mono] indicates that a polymorphic type appeared where a
  monomorphic type was expected.
 *)
exception Expected_mono

(** {1 Constructors and Destructors} *)

(** [var rank id] constructs a variable with the identifier [id]. *)
val var : int -> Identifier.t -> t

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

(** [get_quants tp] computes the quantified type variables in [tp]. *)
val get_quants : t -> Identifier.t list

(** {1 Setters} *)

val set_rank : int -> t -> unit

(** {1 Inference} *)

(** [unify tp1 tp2] unifies [tp1] and [tp2]. *)
val unify : t -> t -> unit

(**
  [gen r tp] replaces all monomorphic variables [tp] of rank greater
  than [r] with polymorphic variables.
 *)
val gen : int -> t -> t

(**
  [inst r tp] replaces all polymorphic variables in [tp] with fresh
  monomorphic variables of rank [r].  The result is a pair containing
  the monomorphized type, along with a list of the fresh monomorphic
  variables.
 *)
val inst : int -> t -> t list * t

(** {1 Utilities} *)

(**
  [to_intl_repr tp] computes an internal representation type which is
  equivalent to [tp].
 *)
val to_intl_repr : t -> Universal_types.Type.t

(**
  [simplify tp] replaces each variable in [tp] with the
  lexicographically lowest unused variable.
 *)
val simplify : t -> t

(**
  [to_string tp] computes a string representation of [tp].  Unless
  [no_simp] is passed, [tp] is simplified first.  Unless [show_quants]
  is passed, the universal quantifiers are not printed.
 *)
val to_string : ?no_simp : unit -> ?show_quants : unit -> t -> string
