module type Sig = sig

  module Value : sig

    type t

    module Environment : sig

      type env

      val initial : env

      val add : Identifier.t -> t -> env -> env

    end

    val to_string : t -> string

  end

  module Kind : sig

    type t

    module Environment : sig

      type env

      val initial : env

      val add : Identifier.t -> t -> env -> env

    end

    val to_string : t -> string

  end

  module Type : sig

    type t

    module Environment : sig

      type env

      val initial : env

      val add_term : Identifier.t -> t -> env -> env

      val add_type : Identifier.t -> t -> env -> env

    end

    val to_kind : Kind.Environment.env -> t -> Kind.t

    val beta_reduce : ?deep : unit -> Environment.env -> t -> t

    val to_string : t -> string

  end

  module Term : sig

    type t

    val to_type :
      (Kind.Environment.env * Type.Environment.env) ->
      t ->
      Type.t

    val to_value :
      ?deep : unit ->
      ( Value.Environment.env *
        Kind.Environment.env *
        Type.Environment.env ) ->
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
