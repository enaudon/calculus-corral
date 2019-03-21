module type Input = sig

  module Value : sig

    type t

    module Environment : Environment.Output with type value := t

    val to_string : t -> string

  end

  module Kind : sig

    type t

    module Environment : Environment.Output with type value := t

    val to_string : t -> string

  end

  module Type : sig

    type t

    module Environment : Type_environment.Output with type value := t

    val to_kind : Kind.Environment.t -> t -> Kind.t

    val beta_reduce : ?deep : unit -> Environment.t -> t -> t

    val to_string : t -> string

  end

  module Term : sig

    type t

    val to_type :
      (Kind.Environment.t * Type.Environment.t) ->
      t ->
      Type.t

    val to_value :
      ?deep : unit ->
      ( Value.Environment.t *
        Kind.Environment.t *
        Type.Environment.t ) ->
      t ->
      Value.t

    val to_string : t -> string

  end

  val parse : Lexing.lexbuf -> (Type.t, Term.t) Command.t list

  val arg_specs : (Arg.key * Arg.spec * Arg.doc) list

end

module Repl (Input : Input) : sig

  (** [main ()] is the function where the program starts. *)
  val main : unit -> unit

end
