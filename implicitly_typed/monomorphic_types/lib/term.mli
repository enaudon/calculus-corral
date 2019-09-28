(** {1 Types} *)

(** The type of terms. *)
type t

(** {1 Constructors} *)

(** [var id] constructs a variable with the identifier [id]. *)
val var : ?loc:Location.t -> Identifier.t -> t

(** [abs args body] constructs the abstraction of [arg] from [body]. *)
val abs : ?loc:Location.t -> Identifier.t -> t -> t

(** [abs' args body] constructs the abstraction of [args] from [body]. *)
val abs' : ?loc:Location.t -> Identifier.t list -> t -> t

(** [app fn arg] constructs the application of [fn] to [arg]. *)
val app : ?loc:Location.t -> t -> t -> t

(** [app' fn arg] constructs the application of [fn] to [args]. *)
val app' : ?loc:Location.t -> t -> t list -> t

(** {1 Typing} *)

(** [to_type_hm env tm] computes the type of [tm] under [env], via Algorithm
    W-style Hindley-Milner type inference. *)
val to_type_hm : Type.Environment.t -> t -> Type.t

(** [to_type_pr env tm] computes the type of [tm] under [env], via
    constraint-based type inference a la Pottier and Remy. *)
val to_type_pr : Type.Environment.t -> t -> Type.t

(** {1 Utilities} *)

(** [to_string tm] computes a string representation of [tm]. *)
val to_string : t -> string
