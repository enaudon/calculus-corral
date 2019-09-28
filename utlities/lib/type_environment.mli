(** Type Environment

    The type environment tracks bindings from term and type identifiers. *)

(** The input signature of the type environment functor. *)
module type Input = sig
  (** The type of bound values. *)
  type value

  (** The type bindings in the initial environment. *)
  val initial_types : (Identifier.t * value) list

  (** The term bindings in the initial environment. *)
  val initial_terms : (Identifier.t * value) list
end

(** The output signature of the type environment functor. *)
module type Output = sig
  (** {1 Types} *)

  (** The type of bound values. *)
  type value

  (** The type of environments. *)
  type t

  (** {1 Values} *)

  (** [empty] is the empty environment. *)
  val empty : t

  (** [initial] is the initial environment. *)
  val initial : t

  (** Type Identifier Functions *)
  module Type : sig
    (** [singleton id v] constructs an environment with the type identifier,
        [id], bound to [v]. *)
    val singleton : Identifier.t -> value -> t

    (** [add id tp env] extends [env] with a binding from the type identifier,
        [id], to [tp]. *)
    val add : Identifier.t -> value -> t -> t

    (** [del id env] from [env] removes the binding from the type identifier,
        [id]. *)
    val del : Identifier.t -> t -> t

    (** [find id env] computes the value to which the type identifier [id] is
        bound in [env]. If no such value exists, [find] raises
        [Identifier.Map.Unbound id]. *)
    val find : Identifier.t -> t -> value

    (** [find_default tp id env] computes the value to which the type
        identifier, [id], is bound in [env]. If no such value exists, [find]
        evaluates to [tp]. *)
    val find_default : value -> Identifier.t -> t -> value

    (** [mem id env] determines whether the type identifier, [id] is bound in
        [env]. *)
    val mem : Identifier.t -> t -> bool

    (** [bindings env] computes a list of the type bindings in [env]. *)
    val bindings : t -> (Identifier.t * value) list

    (** [keys env] computes a list of the bound type identifiers in [env]. *)
    val keys : t -> Identifier.t list

    (** [values env] computes a list of the type values in [env]. *)
    val values : t -> value list

    (** [fold fn env init] computes [fn idN vN (... (fn id0 v0 init)...)], where
        the [id]'s and [v]'s are the type bindings in [env] in increasing order. *)
    val fold : (Identifier.t -> value -> 'a -> 'a) -> t -> 'a -> 'a
  end

  (** Term Identifier Functions *)
  module Term : sig
    (** [singleton id v] constructs an environment with the term identifier,
        [id], bound to [v]. *)
    val singleton : Identifier.t -> value -> t

    (** [add id tp env] extends [env] with a binding from the term identifier,
        [id], to [tp]. *)
    val add : Identifier.t -> value -> t -> t

    (** [del id env] from [env] removes the binding from the term identifier,
        [id]. *)
    val del : Identifier.t -> t -> t

    (** [find id env] computes the value to which the term identifier [id] is
        bound in [env]. If no such value exists, [find] raises
        [Identifier.Map.Unbound id]. *)
    val find : Identifier.t -> t -> value

    (** [find_default tp id env] computes the value to which the term
        identifier, [id], is bound in [env]. If no such value exists, [find]
        evaluates to [tp]. *)
    val find_default : value -> Identifier.t -> t -> value

    (** [mem id env] determines whether the term identifier, [id] is bound in
        [env]. *)
    val mem : Identifier.t -> t -> bool

    (** [bindings env] computes a list of the term bindings in [env]. *)
    val bindings : t -> (Identifier.t * value) list

    (** [keys env] computes a list of the bound term identifiers in [env]. *)
    val keys : t -> Identifier.t list

    (** [values env] computes a list of the term values in [env]. *)
    val values : t -> value list

    (** [fold fn env init] computes [fn idN vN (... (fn id0 v0 init)...)], where
        the [id]'s and [v]'s are the term bindings in [env] in increasing order. *)
    val fold : (Identifier.t -> value -> 'a -> 'a) -> t -> 'a -> 'a
  end

  (** {1 Other Functions} *)

  (** [of_list tps tms] constructs an environment from the type and term
      bindings in [tps] and [tms], respectively. *)
  val of_list : (Identifier.t * value) list -> (Identifier.t * value) list -> t

  (** [bindings env] computes a list of the type and term bindings in [env]. *)
  val bindings : t -> (Identifier.t * value) list

  (** [fold tp_fn tm_fn env init] computes
      [tm_fn idN vN (... (tp_fn id0 v0 init)...)], where the [id]'s and [v]'s
      are the type and term bindings in [env] in increasing order. *)
  val fold :
    (Identifier.t -> value -> 'a -> 'a) ->
    (Identifier.t -> value -> 'a -> 'a) ->
    t ->
    'a ->
    'a
end

module Make (Input : Input) : Output with type value := Input.value
