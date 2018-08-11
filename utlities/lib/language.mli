module type Sig = sig

  module Value : sig

    type t

    val to_string : t -> string

  end

  module Kind : sig

    type t

    val to_string : t -> string

  end

  module Type : sig

    type t

    val default_env : Kind.t Identifier.Map.t

    val to_kind : ?env : Kind.t Identifier.Map.t -> t -> Kind.t

    val beta_reduce :
      ?deep : unit -> ?env : t Identifier.Map.t -> t -> t

    val to_string : t -> string

  end

  module Term : sig

    type t

    val to_type :
      ?env : (Kind.t Identifier.Map.t * Type.t Identifier.Map.t) ->
      t ->
      Type.t

    val to_value :
      ?deep : unit ->
      ?env :
        ( Value.t Identifier.Map.t *
          Kind.t Identifier.Map.t *
          Type.t Identifier.Map.t ) ->
      t ->
      Value.t

    val to_string : t -> string

  end

  val parse : Lexing.lexbuf -> (Type.t, Term.t) Command.t list

  val arg_specs : (Arg.key * Arg.spec * Arg.doc) list

end

module Repl (Input : Sig) : sig

  (** [main ()] is the function where the program starts. *)
  val main : unit -> unit

end
