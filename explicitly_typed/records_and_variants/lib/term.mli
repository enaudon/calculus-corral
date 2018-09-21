(** {1 Types} *)

(** The type of terms. *)
type t

(** {1 Constructors} *)

(** [var id] constructs a variable with the identifier [id]. *)
val var : ?loc : Location.t -> Identifier.t -> t

(**
  [abs arg tp body] constructs the abstraction of [arg] of type [tp]
  from [body].
 *)
val abs : ?loc : Location.t -> Identifier.t -> Type.t -> t -> t

(**
  [abs' args body] constructs the abstraction of [args] from [body].
 *)
val abs' : ?loc : Location.t -> (Identifier.t * Type.t) list -> t -> t

(** [app fn arg] constructs the application of [fn] to [arg]. *)
val app : ?loc : Location.t -> t -> t -> t

(** [app' fn arg] constructs the application of [fn] to [args]. *)
val app' : ?loc : Location.t -> t -> t list -> t

(**
  [tp_abs arg body] constructs the abstraction of [arg] from [body].
 *)
val tp_abs : ?loc : Location.t -> Identifier.t -> t -> t

(**
  [abs' args body] constructs the abstraction of [args] from [body].
 *)
val tp_abs' : ?loc : Location.t -> Identifier.t list -> t -> t

(** [tp_app fn arg] constructs the application of [fn] to [arg]. *)
val tp_app : ?loc : Location.t -> t -> Type.t -> t

(** [tp_app' fn arg] constructs the application of [fn] to [args]. *)
val tp_app' : ?loc : Location.t -> t -> Type.t list -> t

(** [rcrd fields] constructs a record term. *)
val rcrd : ?loc : Location.t -> (Identifier.t * t) list -> t

(** [proj rcrd field] constructs a projection term. *)
val proj : ?loc : Location.t -> t -> Identifier.t -> t

(** [vnrt case data tp] constructs a variant term. *)
val vrnt : ?loc : Location.t -> Identifier.t -> t -> Type.t -> t

(** [case vrnt cases] constructs a case term. *)
val case :
  ?loc : Location.t -> t -> (Identifier.t * Identifier.t * t) list -> t

(** {1 Typing} *)

(** [to_type env tm] computes the type of [tm] under [env]. *)
val to_type :
  (Identifier.Set.t * Type.t Identifier.Map.t) -> t -> Type.t

(** {1 Transformations} *)

(**
  [beta_reduce ~deep:() env tm] evaluates any applications in [tm] under
  [env]. If the [deep] argument is passed, then [beta_reduce] will
  evaluate the body of abstractions.
 *)
val beta_reduce : ?deep : unit -> t Identifier.Map.t -> t -> t

(** {1 Utilities} *)

(**
  [alpha_equivalent tm1 tm2] determines whether [tm1] and [tm2] are
  equivalent up to renaming of variables.
 *)
val alpha_equivalent : t -> t -> bool

(**
  [simplify tm] replaces each type variable in [tm] with the
  lexicographically lowest unused variable.
 *)
val simplify : t -> t

(** [to_string tm] computes a string representation of [tm]. *)
val to_string : t -> string
