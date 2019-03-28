(** {1 Types} *)

(** The type of kinds. *)
type t

(** {1 Containers} *)

(** Environment of bound types. *)
module Environment : Environment.Output with type value := t

(** {1 Constructors and Destructors} *)

(** [prop] is the proper kind. *)
val prop : t

(** [row] is the row kind. *)
val row : t

(** [oper arg res] constructs an operator from [arg] to [res]. *)
val oper : t -> t -> t

(** [oper' args res] constructs an operator from [args] to [res]. *)
val oper' : t list -> t -> t

(**
  [get_oper kn] computes the argument and result kind of [kn], if [kn]
  is an operator.  Otherwise, [get_oper] raises [Invalid_argument].
 *)
val get_oper : t -> t * t

(** {1 Utilities} *)

(**
  [to_intl_repr kn] computes an internal representation type which is
  equivalent to [kn].
 *)
val to_intl_repr : t -> Records_and_variants.Kind.t

(**
  [alpha_equivalent kn1 kn2] determines whether [kn1] and [kn2] are
  equivalent up to renaming of variables.
 *)
val alpha_equivalent : t -> t -> bool

(** [to_string kn] computes a string representation of [kn]. *)
val to_string : t -> string
