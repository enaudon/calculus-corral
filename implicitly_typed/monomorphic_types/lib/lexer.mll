{

let skip_line = Lexing.new_line

let skip_char lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_cnum = pos.pos_cnum + 1 }

}

let whitespace = [' ' '\009' '\012']
let newline = ("\r")?"\n"
let id_char =  ['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']
let lower_id = ['a'-'z'] id_char*

rule prog = parse
  | whitespace                        { skip_char lexbuf; prog lexbuf }
  | newline                           { skip_line lexbuf; prog lexbuf }
  | "->"                              { Parser.ARROW }
  | "."                               { Parser.PERIOD }
  | ";"                               { Parser.SEMICOLON }
  | "("                               { Parser.O_PAREN }
  | ")"                               { Parser.C_PAREN }
  | lower_id as id                    { Parser.LOWER_ID id }
  | _ as c                            { failwith @@
                                          Printf.sprintf
                                            "Lexing error -- unexpected character '%c'"
                                            c }
