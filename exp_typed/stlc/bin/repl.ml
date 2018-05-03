open Stlc

module Id = Identifier

type mode =
  | Repl
  | File of string list

let mode = ref Repl

let parse_cmd_args () =
  let usage_msg = Printf.sprintf "Usage: %s [file]" Sys.argv.(0) in
  let parse_file f = match !mode with
    | Repl -> mode := File [f]
    | File fs -> mode := File (f :: fs)
  in
  Arg.parse [] parse_file usage_msg

let evaluate env lexbuf =
  try
    match Parser.command Lexer.prog lexbuf with
      | Command.Bind_term (id, tm) ->
        let tp = Term.to_type ~env tm in
        Printf.printf "%s\n  : %s\n  = %s ;\n%!"
          (Id.to_string id)
          (Type.to_string tp)
          (Term.to_string tm);
        Id.Map.add id (Term.to_type tm) env
      | Command.Eval_term tm ->
        let tp = Term.to_type ~env tm in
        let vl = Term.beta_reduce tm in
        Printf.printf "%s\n  : %s\n  = %s ;\n%!"
          (Term.to_string tm)
          (Type.to_string tp)
          (Term.to_string vl);
        env
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
      let rec eval_phrase (env : Type.t Id.Map.t) =
        Printf.printf "> %!";
        let env' =
          evaluate env (Lexing.from_string @@ input_line stdin)
        in
        eval_phrase env'
      in
      eval_phrase Id.Map.empty
    | File fs ->
      let eval_file f =
        let chan = open_in f in
        ignore (evaluate Id.Map.empty @@ Lexing.from_channel chan);
        close_in chan;
      in
      List.iter eval_file @@ List.rev fs

let () = main ()
