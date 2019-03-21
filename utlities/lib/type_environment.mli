(** Type Environment

  The type environment tracks bindings from term and type identifiers.
 *)

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

  (** {1 Type Identifier Functions} *)

  (**
    [singleton id v] constructs an environment with the type
    identifier, [id], bound to [v].
   *)
  val singleton_type : Identifier.t -> value -> t
  
  (**
    [add_type id tp env] extends [env] with a binding from the type
    identifier, [id], to [tp].
   *)
  val add_type : Identifier.t -> value -> t -> t
  
  (**
    [del_type id env] from [env] removes the binding from the type
    identifier, [id].
   *)
  val del_type : Identifier.t -> t -> t
  
  (**
    [find_type id env] computes the value to which the type identifier
    [id] is bound in [env].  If no such value exists, [find] raises
    [Identifier.Map.Unbound id].
   *)
  val find_type : Identifier.t -> t -> value

  (**
    [find_default_type tp id env] computes the value to which the type
    identifier, [id], is bound in [env].  If no such value exists,
    [find] evaluates to [tp].
   *)
  val find_default_type : value -> Identifier.t -> t -> value

  (**
    [mem_type id env] determines whether the type identifier, [id] is
    bound in [env].
   *)
  val mem_type : Identifier.t -> t -> bool

  (**
    [type_bindings env] computes a list of the type bindings in [env].
   *)
  val type_bindings : t -> (Identifier.t * value) list

  (**
    [type_keys env] computes a list of the bound type identifiers in
    [env].
   *)
  val type_keys : t -> Identifier.t list

  (** [type_values env] computes a list of the type values in [env]. *)
  val type_values : t -> value list

  (**
    [fold_type fn env init] computes
    [fn idN vN (... (fn id0 v0 init)...)],
    where the [id]'s and [v]'s are the type bindings in [env] in
    increasing order.
   *)
  val fold_type :
    (Identifier.t -> value -> 'a -> 'a) -> t -> 'a -> 'a

  (** {1 Term Identifier Functions} *)

  (**
    [singleton id v] constructs an environment with the term
    identifier, [id], bound to [v].
   *)
  val singleton_term : Identifier.t -> value -> t
  
  (**
    [add_term id tp env] extends [env] with a binding from the term
    identifier, [id], to [tp].
   *)
  val add_term : Identifier.t -> value -> t -> t
  
  (**
    [del_term id env] from [env] removes the binding from the term
    identifier, [id].
   *)
  val del_term : Identifier.t -> t -> t
  
  (**
    [find_term id env] computes the value to which the term identifier
    [id] is bound in [env].  If no such value exists, [find] raises
    [Identifier.Map.Unbound id].
   *)
  val find_term : Identifier.t -> t -> value

  (**
    [find_default_term tp id env] computes the value to which the term
    identifier, [id], is bound in [env].  If no such value exists,
    [find] evaluates to [tp].
   *)
  val find_default_term : value -> Identifier.t -> t -> value

  (**
    [mem_term id env] determines whether the term identifier, [id] is
    bound in [env].
   *)
  val mem_term : Identifier.t -> t -> bool

  (**
    [term_bindings env] computes a list of the term bindings in [env].
   *)
  val term_bindings : t -> (Identifier.t * value) list

  (**
    [term_keys env] computes a list of the bound term identifiers in
    [env].
   *)
  val term_keys : t -> Identifier.t list

  (** [term_values env] computes a list of the term values in [env]. *)
  val term_values : t -> value list

  (**
    [fold_term fn env init] computes
    [fn idN vN (... (fn id0 v0 init)...)],
    where the [id]'s and [v]'s are the term bindings in [env] in
    increasing order.
   *)
  val fold_term :
    (Identifier.t -> value -> 'a -> 'a) -> t -> 'a -> 'a

  (** {1 Other Functions} *)

  (**
    [of_list tps tms] constructs an environment from the type and term
    bindings in [tps] and [tms], respectively.
   *)
  val of_list :
    (Identifier.t * value) list -> (Identifier.t * value) list -> t

  (**
    [bindings env] computes a list of the type and term bindings in
    [env].
   *)
  val bindings : t -> (Identifier.t * value) list

  (**
    [fold_both tp_fn tm_fn env init] computes
    [tm_fn idN vN (... (tp_fn id0 v0 init)...)],
    where the [id]'s and [v]'s are the type and term bindings in [env]
    in increasing order.
   *)
  val fold_both :
    (Identifier.t -> value -> 'a -> 'a) ->
    (Identifier.t -> value -> 'a -> 'a) ->
    t ->
    'a ->
    'a

end

module Make (Input : Input) : Output with type value := Input.value
