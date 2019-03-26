module type Environment = sig
  type value
  type t
  val initial : t
  val add : Identifier.t -> value -> t -> t
end

module type Type_environment = sig

  type value
  type t

  val initial : t

  module Type : sig
    val add : Identifier.t -> value -> t -> t
  end

  module Term : sig
    val add : Identifier.t -> value -> t -> t
  end

end

module type Input = sig

  module Value : sig
    type t
    module Environment : Environment with type value := t
    val to_string : t -> string
  end

  module Kind : sig
    type t
    module Environment : Environment with type value := t
    val to_string : t -> string
  end

  module Type : sig
    type t
    module Environment : Type_environment with type value := t
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

module Repl (Input : Input) = struct

  open Input

  module Id = Identifier
  module Value_env = Value.Environment
  module Kind_env = Kind.Environment
  module Type_env = Type.Environment

  type mode =
    | Repl
    | File of string list

  let mode = ref Repl

  let deep = ref None

  let parse_cmd_args () =
    let initial_specs = [
      ( "--deep-beta-reduction", Arg.Unit (fun () -> deep := Some ()),
        "Beta-reduce within the body of abstractions." ) ;
    ] in
    let parse_file f = match !mode with
      | Repl -> mode := File [f]
      | File fs -> mode := File (f :: fs)
    in
    let usage_msg = Printf.sprintf "Usage: %s [file]" Sys.argv.(0) in
    Arg.parse (arg_specs @ initial_specs) parse_file usage_msg

  let evaluate kn_env tp_env vl_env lexbuf =

    let deep = !deep in

    let evaluate_command (kn_env, tp_env, vl_env) cmd = match cmd with
      | Command.Bind_type (id, tp) ->
        let kn = Type.to_kind kn_env tp in
        let tp' = Type.beta_reduce ?deep tp_env tp in
        Printf.printf "%s\n  : %s\n  = %s ;\n%!"
          (Id.to_string id)
          (Kind.to_string kn)
          (Type.to_string tp');
        ( Kind_env.add id kn kn_env,
          Type_env.Type.add id tp' tp_env,
          vl_env )
      | Command.Bind_term (id, tm) ->
        let tp = Term.to_type (kn_env, tp_env) tm in
        let vl = Term.to_value ?deep (vl_env, kn_env, tp_env) tm in
        Printf.printf "%s\n  : %s\n  = %s ;\n%!"
          (Id.to_string id)
          (Type.to_string tp)
          (Value.to_string vl);
        ( kn_env,
          Type_env.Term.add id tp tp_env,
          Value_env.add id vl vl_env )
      | Command.Eval_term tm ->
        let tp = Term.to_type (kn_env, tp_env) tm in
        let vl = Term.to_value ?deep (vl_env, kn_env, tp_env) tm in
        Printf.printf "%s\n  : %s\n  = %s ;\n%!"
          (Term.to_string tm)
          (Type.to_string tp)
          (Value.to_string vl);
        kn_env, tp_env, vl_env
    in

    try
      List.fold_left
        evaluate_command
        (kn_env, tp_env, vl_env)
        (parse lexbuf)
    with
      | Parsing.Parse_error ->
        Printf.eprintf "Parser: error\n%!";
        kn_env, tp_env, vl_env
      | Failure msg ->
        Printf.eprintf "%s\n%!" msg;
        kn_env, tp_env, vl_env

  let main () =

    parse_cmd_args ();

    let lexbuf_set_filename lexbuf f =
      let open Lexing in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = f };
    in

    match !mode with
      | Repl ->
        let rec eval_phrase kn_env tp_env vl_env =
          Printf.printf "> %!";
          let lexbuf = Lexing.from_string @@ input_line stdin in
          lexbuf_set_filename lexbuf "<repl>";
          let kn_env', tp_env', vl_env' =
            evaluate kn_env tp_env vl_env lexbuf
          in
          eval_phrase kn_env' tp_env' vl_env'
        in
        eval_phrase Kind_env.initial Type_env.initial Value_env.initial
      | File fs ->
        let eval_file f (kn_env, tp_env, vl_env) =
          let chan = open_in f in
          let lexbuf = Lexing.from_channel chan in
          lexbuf_set_filename lexbuf f;
          let envs = evaluate kn_env tp_env vl_env lexbuf in
          close_in chan;
          envs
        in
        let envs =
          (Kind_env.initial, Type_env.initial, Value_env.initial)
        in
        ignore @@ List.fold_right eval_file fs envs

end
