(** {1 Types} *)

(** The type of types. *)
type t

(** {1 Containers} *)

(** Environment of bound types. *)
module Environment : Type_environment.Output with type value := t

(** {1 Exceptions} *)

(**
  [Occurs (id, tp)] indicates that unification failed because the type
  variable identified by [id] occurs in the type [tp].
 *)
exception Occurs of Identifier.t * t

(** {1 Constructors} *)

(** [var id] constructs a variable with the identifier [id]. *)
val var : Identifier.t -> t

(** [func arg res] constructs a function from [arg] to [res]. *)
val func : t -> t -> t

(** [func' args res] constructs a function from [args] to [res]. *)
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
  [tp2].  In cases where both [tp1] and [tp2] are variables, and either
  identifier may be kept in the substitution, [tp1]'s identifier is
  kept.
 *)
val unify : Substitution.s -> t -> t -> Substitution.s

(** {1 Utilities} *)

(**
  [simplify tp] replaces each variable in [tp] with the
  lexicographically lowest unused variable.
 *)
val simplify : t -> t

(**
  [to_string ~no_simp tp] computes a string representation of [tp].
  Unless [no_simp] is passed, [tp] is simplified first.
 *)
val to_string : ?no_simp : unit -> t -> string
