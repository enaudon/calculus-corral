(** {1 Types} *)

(** The type of identifiers. *)
type t

(** {1 Exceptions} *)

(** [Unbound id] indicates that no binding exists for the [id]. *)
exception Unbound of t

(** {1 Maps} *)

(** Maps from identifiers. *)
module Map : sig

  (** The type of map keys, i.e. identifiers. *)
  type key = t

  (** The type of maps from identifiers. *)
  type 'a t

  (** [empty] is the empty map. *)
  val empty : 'a t

  (** [add id x map] extends [env] with a binding from [id] to [x]. *)
  val add : key -> 'a -> 'a t -> 'a t

  (**
    [find id map] computes the value to which [id] is bound in [env].
    If no such value exists, then [find] raises [Unbound id].
   *)
  val find : key -> 'a t -> 'a

end

(** {1 Functions} *)

(** [fresh ()] produces a fresh identifier. *)
val fresh : unit -> t

(**
  [reset ()] resets the internal data structures for generating fresh
  identifiers.
 *)
val reset : unit -> unit

(** [of_string str] creates an identifier from the string [str]. *)
val of_string : string -> t

(** [to_string id] computes a string representation of [id]. *)
val to_string : t -> string
