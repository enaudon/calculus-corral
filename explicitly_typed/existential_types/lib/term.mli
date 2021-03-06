(** {1 Types} *)

(** The type of terms. *)
type t

(** {1 Containers} *)

(** Environment of bound terms. *)
module Environment : Environment.Output with type value := t

(** {1 Constructors} *)

(** [var id] constructs a variable with the identifier [id]. *)
val var : ?loc:Location.t -> Identifier.t -> t

(** [abs arg tp body] constructs the abstraction of [arg] of type [tp] from
    [body]. *)
val abs : ?loc:Location.t -> Identifier.t -> Type.t -> t -> t

(** [abs' args body] constructs the abstraction of [args] from [body]. *)
val abs' : ?loc:Location.t -> (Identifier.t * Type.t) list -> t -> t

(** [app fn arg] constructs the application of [fn] to [arg]. *)
val app : ?loc:Location.t -> t -> t -> t

(** [app' fn arg] constructs the application of [fn] to [args]. *)
val app' : ?loc:Location.t -> t -> t list -> t

(** [pack tp1 tm tp2] constructs a term of the form [pack tp1, tm as tp2]. *)
val pack : ?loc:Location.t -> Type.t -> t -> Type.t -> t

(** [unpack tp_id tm_id pack body] constructs a term of the form
    [unpack tp_id, tm_id = pack in body]. *)
val unpack : ?loc:Location.t -> Identifier.t -> Identifier.t -> t -> t -> t

(** {1 Typing} *)

(** [to_type env tm] computes the type of [tm] under [env]. *)
val to_type : Identifier.Set.t * Type.Environment.t -> t -> Type.t

(** {1 Transformations} *)

(** [beta_reduce ~deep:() env tm] evaluates any applications in [tm] under
    [env]. If the [deep] argument is passed, then [beta_reduce] will evaluate
    the body of abstractions. *)
val beta_reduce : ?deep:unit -> Type.Environment.t * Environment.t -> t -> t

(** {1 Utilities} *)

(** [alpha_equivalent tm1 tm2] determines whether [tm1] and [tm2] are equivalent
    up to renaming of variables. *)
val alpha_equivalent : t -> t -> bool

(** [simplify tm] replaces each type variable in [tm] with the lexicographically
    lowest unused variable. *)
val simplify : t -> t

(** [to_string tm] computes a string representation of [tm]. *)
val to_string : t -> string
