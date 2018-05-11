module type Sig = sig

  module Kind : sig

    type t

    val to_string : t -> string

  end

  module Type : sig

    type t

    val to_kind : ?env : Kind.t Identifier.Map.t -> t -> Kind.t

    val beta_reduce :
      ?deep : unit -> ?env : t Identifier.Map.t -> t -> t

    val to_string : t -> string

  end

  module Term : sig

    type t

    val to_type : ?env : Type.t Identifier.Map.t -> t -> Type.t

    val beta_reduce : ?deep : unit -> ?env : t Identifier.Map.t -> t -> t

    val to_string : t -> string

  end

  val parse : Lexing.lexbuf -> (Type.t, Term.t) Command.t list

end

module Repl (Input : Sig) : sig

  (** [main ()] is the function where the program starts. *)
  val main : unit -> unit

end
