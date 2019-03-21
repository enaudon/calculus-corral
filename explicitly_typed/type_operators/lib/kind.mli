(** {1 Types} *)

(** The type of kinds. *)
type t

(** {1 Environments} *)

(** [initial_env] is the initial kind environment. *)
val initial_env : (Identifier.t * t) list

(** {1 Constructors and Destructors} *)

(** [prop] is the proper kind. *)
val prop : t

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
  [alpha_equivalent kn1 kn2] determines whether [kn1] and [kn2] are
  equivalent up to renaming of variables.
 *)
val alpha_equivalent : t -> t -> bool

(** [to_string kn] computes a string representation of [kn]. *)
val to_string : t -> string
