(** {1 Types} *)

(** The type of terms. *)
type t

(** {1 Containers} *)

(** Environment of bound terms. *)
module Environment : Environment.Output with type value := t

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

(** {1 Typing} *)

(** [to_type env tm] computes the type of [tm] under [env]. *)
val to_type : (Identifier.Set.t * Type.Environment.t) -> t -> Type.t

(** {1 Transformations} *)

(**
  [beta_reduce ~deep:() env tm] evaluates any applications in [tm] under
  [env]. If the [deep] argument is passed, then [beta_reduce] will
  evaluate the body of abstractions.
 *)
val beta_reduce :
  ?deep : unit -> (Type.Environment.t * Environment.t) -> t -> t

(** {1 Utilities} *)

(**
  [alpha_equivalent tm1 tm2] determines whether [tm1] and [tm2] are
  equivalent up to renaming of variables.
 *)
val alpha_equivalent : t -> t -> bool

(**
  [subst_tp fvs sub tp] applies [sub] to [tp], replacing any variable in
  the domain of [sub] with the corresponding type the range of [sub].
  [fvs] is any superset of the variables which appear in the range of
  [sub].
 *)
val subst_tp : Identifier.Set.t -> Type.Environment.t -> t -> t

(**
  [subst_tm fvs sub tm] applies [sub] to [tm], replacing any variable in
  the domain of [sub] with the corresponding term the range of [sub].
  [fvs] is any superset of the variables which appear in the range of
  [sub].
 *)
val subst_tm : Identifier.Set.t -> Environment.t -> t -> t

(**
  [simplify tm] replaces each type variable in [tm] with the
  lexicographically lowest unused variable.
 *)
val simplify : t -> t

(** [to_string tm] computes a string representation of [tm]. *)
val to_string : t -> string
