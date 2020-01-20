(** {1 Types} *)

(** The type of types. *)
type t

(** {1 Containers} *)

(** Environment of bound types. *)
module Environment : Type_environment.Output with type value := t

(** {1 Exceptions} *)

(** [Occurs (id, tp)] indicates that unification failed because the inference
    variable identified by [id] occurs in the type [tp]. *)
exception Occurs of Identifier.t * t

(** {1 Constructors} *)

(** [inf_var id] constructs an inference variable with the identifier [id]. *)
val inf_var : Identifier.t -> t

(** [func arg res] constructs a function from [arg] to [res]. *)
val func : t -> t -> t

(** [func' args res] constructs a function from [args] to [res]. *)
val func' : t list -> t -> t

(** {1 Inference} *)

(** Inferencer

    This module contains type inference functionality, such as unification.
    After inference is complete, the [apply] function will apply the
    substitution computed by type inference. *)
module Inferencer : sig
  (** The type of inference engine state. *)
  type state

  (** [initial] is the initial state. *)
  val initial : state

  (** [unify sub tp1 tp2] computes the subtitution which unifies [tp1] and
      [tp2]. In cases where both [tp1] and [tp2] are inference variables, and
      either identifier may be kept in the substitution, [tp1]'s identifier is
      kept. *)
  val unify : state -> t -> t -> state

  (** [apply tp state] applies the substitution in [state] to [tp], replacing
      any inference variables in [tp] which occur in the domain of the
      substitution with their corresponding concrete types in the range of the
      substitution. *)
  val apply : state -> t -> t
end

(** {1 Utilities} *)

(** [simplify tp] replaces each inference variable in [tp] with the
    lexicographically lowest unused variable.

    NOTE: [simplify]'d types cannot be used with inference functions. *)
val simplify : t -> t

(** [to_string ~no_simp tp] computes a string representation of [tp]. Unless
    [no_simp] is passed, [tp] is simplified first. *)
val to_string : ?no_simp:unit -> t -> string
