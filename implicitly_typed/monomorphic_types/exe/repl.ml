open Mono

module Id = Identifier

let main () = 

  let rec loop () = 
    Printf.printf "> %!";
    let lexbuf = Lexing.from_string (input_line stdin) in
    begin try
      let tm = Parser.term Lexer.prog lexbuf in
      let tp = Term.to_type_pr tm in
      Printf.printf "%s : %s\n%!"
        (Term.to_string tm)
        (Type.to_string tp);
    with
      | Parsing.Parse_error -> Printf.printf "Parsing error\n%!"
      | Failure msg -> Printf.printf "%s\n%!" msg
    end;
    loop ()
  in

  loop ()

let () = main ()
