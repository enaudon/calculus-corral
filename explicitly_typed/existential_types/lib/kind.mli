(** {1 Types} *)

(** The type of kinds. *)
type t

(** {1 Constructors and Destructors} *)

(** [base] is the base kind. *)
val base : t

(** {1 Utilities} *)

(** [alpha_equivalent kn1 kn2] determines whether [kn1] and [kn2] are equivalent
    up to renaming of variables. *)
val alpha_equivalent : t -> t -> bool

(** [to_string kn] computes a string representation of [kn]. *)
val to_string : t -> string
