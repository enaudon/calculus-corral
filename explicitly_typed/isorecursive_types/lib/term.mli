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
  [tp_abs arg kn body] constructs the abstraction of [arg] of kind [kn]
  from [body].
 *)
val tp_abs : ?loc : Location.t -> Identifier.t -> Kind.t -> t -> t

(**
  [tp_abs' args kn body] constructs the abstraction of [args] from
  [body].
 *)
val tp_abs' :
  ?loc : Location.t -> (Identifier.t * Kind.t) list -> t -> t

(** [tp_app fn arg] constructs the application of [fn] to [arg]. *)
val tp_app : ?loc : Location.t -> t -> Type.t -> t

(** [tp_app' fn arg] constructs the application of [fn] to [args]. *)
val tp_app' : ?loc : Location.t -> t -> Type.t list -> t

(**
  [roll tp tm] constructs the application of [roll] to [tp] and [tm].
 *)
val roll : ?loc : Location.t -> Type.t -> t -> t

(** [unroll tm] constructs the application of [unroll] to [tm]. *)
val unroll : ?loc : Location.t -> t -> t

(** [fix tm] constructs the application of [fix] to [tm]. *)
val fix : ?loc : Location.t -> t -> t

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
val to_type : (Kind.Environment.t * Type.Environment.t) -> t -> Type.t

(** {1 Transformations} *)

(**
  [beta_reduce ~deep:() env tm] evaluates any applications in [tm] under
  [env]. If the [deep] argument is passed, then [beta_reduce] will
  evaluate the body of abstractions.
 *)
val beta_reduce : ?deep : unit -> Environment.t -> t -> t

(** {1 Utilities} *)

(**
  [alpha_equivalent tm1 tm2] determines whether [tm1] and [tm2] are
  equivalent up to renaming of variables.
 *)
val alpha_equivalent : t -> t -> bool

(* [free_vars tm] computes the free term variables in [tm]. *)
val free_vars : t -> Identifier.Set.t

(**
  [subst_tp tm sub] applies the substitution [sub], which maps
  identifiers to terms, to [tp].
*)
val subst_tp : Identifier.Set.t -> Type.Environment.t -> t -> t

(**
  [subst_tm tm sub] applies the substitution [sub], which maps
  identifiers to terms, to [tm].
*)
val subst_tm : Identifier.Set.t -> Environment.t -> t -> t

(**
  [simplify tm] replaces each type variable in [tm] with the
  lexicographically lowest unused variable.
 *)
val simplify : t -> t

(** [to_string tm] computes a string representation of [tm]. *)
val to_string : t -> string
