open Stlc

module Id = Identifier

type mode =
  | Repl
  | File of string list

let mode = ref Repl

let deep = ref None

let parse_cmd_args () =
  let specs = [
    ("--deep-beta-reduction", Arg.Unit (fun () -> deep := Some ()),
      "Beta-reduce within the body of abstractions.") ;
  ] in
  let parse_file f = match !mode with
    | Repl -> mode := File [f]
    | File fs -> mode := File (f :: fs)
  in
  let usage_msg = Printf.sprintf "Usage: %s [file]" Sys.argv.(0) in
  Arg.parse specs parse_file usage_msg

let evaluate tp_env vl_env lexbuf =

  let deep = !deep in

  let evaluate_command (tp_env, vl_env) cmd = match cmd with
    | Command.Bind_term (id, tm) ->
      let tp = Term.to_type ~env:tp_env tm in
      let vl = Term.beta_reduce ?deep ~env:vl_env tm in
      Printf.printf "%s\n  : %s\n  = %s ;\n%!"
        (Id.to_string id)
        (Type.to_string tp)
        (Term.to_string vl);
      Id.Map.add id tp tp_env, Id.Map.add id vl vl_env
    | Command.Eval_term tm ->
      let tp = Term.to_type ~env:tp_env tm in
      let vl = Term.beta_reduce ?deep ~env:vl_env tm in
      Printf.printf "%s\n  : %s\n  = %s ;\n%!"
        (Term.to_string tm)
        (Type.to_string tp)
        (Term.to_string vl);
      (tp_env, vl_env)
  in

  try
    let cmds = Parser.commands Lexer.prog lexbuf in
    List.fold_left evaluate_command (tp_env, vl_env) cmds
  with
    | Parsing.Parse_error ->
      Printf.printf "Parser: error\n%!";
      exit (-1)
    | Failure msg ->
      Printf.printf "%s\n%!" msg;
      exit (-1)

let main () =

  parse_cmd_args ();

  match !mode with
    | Repl ->
      let rec eval_phrase tp_env vl_env =
        Printf.printf "> %!";
        let tp_env', vl_env' =
          evaluate tp_env vl_env @@
            Lexing.from_string (input_line stdin)
        in
        eval_phrase tp_env' vl_env'
      in
      eval_phrase Id.Map.empty Id.Map.empty
    | File fs ->
      let eval_file f =
        let chan = open_in f in
        ignore @@
          evaluate Id.Map.empty Id.Map.empty (Lexing.from_channel chan);
        close_in chan;
      in
      List.iter eval_file @@ List.rev fs

let () = main ()
