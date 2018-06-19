(** {1 Types} *)

(** The type of types. *)
type t

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

(** [unify tp1 tp2] unifies [tp1] and [tp2]. *)
val unify : t -> t -> unit

(** {1 Utilities} *)

(**
  [simplify tp] replaces each variable in [tp] with the
  lexicographically lowest unused variable.
 *)
val simplify : t -> t

(**
  [to_string tp] computes a string representation of [tp].  Unless
  [no_simp] is passed, [tp] is simplified first.
 *)
val to_string : ?no_simp : unit -> t -> string
