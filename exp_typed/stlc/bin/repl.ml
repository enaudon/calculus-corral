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

let evaluate lexbuf =
  try
    let tm = Parser.term Lexer.prog lexbuf in
    let tp = Term.to_type tm in
    let vl = Term.beta_reduce tm in
    Printf.printf "%s\n  : %s\n  = %s\n%!"
      (Term.to_string tm)
      (Type.to_string tp)
      (Term.to_string vl);
  with
    | Parsing.Parse_error -> Printf.printf "Parser: error\n%!"
    | Failure msg -> Printf.printf "%s\n%!" msg

let main () =

  parse_cmd_args ();

  match !mode with
    | Repl ->
      let rec eval_phrase () =
        Printf.printf "> %!";
        evaluate (Lexing.from_string @@ input_line stdin);
        eval_phrase ()
      in
      eval_phrase ()
    | File fs ->
      let eval_file f =
        let chan = open_in f in
        evaluate @@ Lexing.from_channel chan;
        close_in chan;
      in
      List.iter eval_file @@ List.rev fs

let () = main ()
