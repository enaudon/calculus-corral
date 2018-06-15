module Id = Identifier

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
    val default_env : Kind.t Id.Map.t
    val to_kind : ?env : Kind.t Id.Map.t -> t -> Kind.t
    val beta_reduce : ?deep : unit -> ?env : t Id.Map.t -> t -> t
    val to_string : t -> string
  end

  module Term : sig
    type t
    val to_type :
      ?env : (Kind.t Id.Map.t * Type.t Id.Map.t) -> t -> Type.t
    val to_value :
      ?deep : unit ->
      ?env : (Value.t Id.Map.t * Kind.t Id.Map.t * Type.t Id.Map.t) ->
      t ->
      Value.t
    val to_string : t -> string
  end

  val parse : Lexing.lexbuf -> (Type.t, Term.t) Command.t list

  val arg_specs : (Arg.key * Arg.spec * Arg.doc) list

end

module Repl (Input : Sig) = struct

  open Input

  type mode =
    | Repl
    | File of string list

  let mode = ref Repl

  let deep = ref None

  let parse_cmd_args () =
    let default_specs = [
      ( "--deep-beta-reduction", Arg.Unit (fun () -> deep := Some ()),
        "Beta-reduce within the body of abstractions." ) ;
    ] in
    let parse_file f = match !mode with
      | Repl -> mode := File [f]
      | File fs -> mode := File (f :: fs)
    in
    let usage_msg = Printf.sprintf "Usage: %s [file]" Sys.argv.(0) in
    Arg.parse (arg_specs @ default_specs) parse_file usage_msg

  let evaluate kn_env tp_env vl_env lexbuf =

    let deep = !deep in

    let evaluate_command (kn_env, tp_env, vl_env) cmd = match cmd with
      | Command.Bind_type (id, tp) ->
        let kn = Type.to_kind ~env:kn_env tp in
        let tp' = Type.beta_reduce ?deep ~env:tp_env tp in
        Printf.printf "%s\n  : %s\n  = %s ;\n%!"
          (Id.to_string id)
          (Kind.to_string kn)
          (Type.to_string tp');
        Id.Map.add id kn kn_env, Id.Map.add id tp' tp_env, vl_env
      | Command.Bind_term (id, tm) ->
        let tp = Term.to_type ~env:(kn_env, tp_env) tm in
        let vl = Term.to_value ?deep ~env:(vl_env, kn_env, tp_env) tm in
        Printf.printf "%s\n  : %s\n  = %s ;\n%!"
          (Id.to_string id)
          (Type.to_string tp)
          (Value.to_string vl);
        kn_env, Id.Map.add id tp tp_env, Id.Map.add id vl vl_env
      | Command.Eval_term tm ->
        let tp = Term.to_type ~env:(kn_env, tp_env) tm in
        let vl = Term.to_value ?deep ~env:(vl_env, kn_env, tp_env) tm in
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
        exit (-1)
      | Failure msg ->
        Printf.eprintf "%s\n%!" msg;
        exit (-1)

  let main () =

    parse_cmd_args ();

    match !mode with
      | Repl ->
        let rec eval_phrase kn_env tp_env vl_env =
          Printf.printf "> %!";
          let kn_env', tp_env', vl_env' =
            evaluate kn_env tp_env vl_env @@
              Lexing.from_string (input_line stdin)
          in
          eval_phrase kn_env' tp_env' vl_env'
        in
        eval_phrase Type.default_env Id.Map.empty Id.Map.empty
      | File fs ->
        let eval_file f =
          let chan = open_in f in
          ignore @@
            evaluate Type.default_env Id.Map.empty Id.Map.empty @@
              Lexing.from_channel chan;
          close_in chan;
        in
        List.iter eval_file @@ List.rev fs

 end
