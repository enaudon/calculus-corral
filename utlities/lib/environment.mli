(** Environment

    The environment tracks bindings from identifiers. *)

(** The input signature of the environment functor. *)
module type Input = sig
  (** The type of bound values. *)
  type value

  (** The bindings in the initial environment. *)
  val initial : (Identifier.t * value) list
end

(** The output signature of the environment functor. *)
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

  (** {1 Functions} *)

  (** [singleton id v] constructs an environment with [id] bound to [v]. *)
  val singleton : Identifier.t -> value -> t

  (** [of_list l] constructs an environment from the bindings in [l]. *)
  val of_list : (Identifier.t * value) list -> t

  (** [add id v env] extends [env] with a binding from [id] to [v]. *)
  val add : Identifier.t -> value -> t -> t

  (** [del id env] removes the binding from [id] from [env]. *)
  val del : Identifier.t -> t -> t

  (** [find id env] computes the value to which [id] is bound in [env]. If no
      such value exists, [find] raises [Identifier.Map.Unbound id]. *)
  val find : Identifier.t -> t -> value

  (** [find_default v id env] computes the value to which [id] is bound in
      [env]. If no such value exists, [find] evaluates to [v]. *)
  val find_default : value -> Identifier.t -> t -> value

  (** [mem id env] determines whether [id] is bound in [env]. *)
  val mem : Identifier.t -> t -> bool

  (** [bindings env] computes a list of the bindings in [env]. *)
  val bindings : t -> (Identifier.t * value) list

  (** [keys env] computes a list of the bound identifiers in [env]. *)
  val keys : t -> Identifier.t list

  (** [values env] computes a list of the values in [env]. *)
  val values : t -> value list

  (** [fold fn env init] computes [fn idN vN (... (fn id0 v0 init)...)], where
      the [id]'s and [v]'s are the bindings in [env] in increasing order. *)
  val fold : (Identifier.t -> value -> 'a -> 'a) -> t -> 'a -> 'a
end

module Make (Input : Input) : Output with type value := Input.value
