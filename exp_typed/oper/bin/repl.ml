open Oper

module Id = Identifier

let main () = 

  let rec loop () = 
    Printf.printf "> %!";
    let lexbuf = Lexing.from_string (input_line stdin) in
    begin try
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
    end;
    loop ()
  in

  loop ()

let () = main ()
