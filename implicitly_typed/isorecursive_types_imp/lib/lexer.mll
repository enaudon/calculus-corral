{

module Loc = Location

let get_loc lexbuf =
  let curr_pos = lexbuf.Lexing.lex_curr_p in
  Loc.of_lex_pos curr_pos curr_pos

let error lexbuf msg =
  failwith @@
    Printf.sprintf "%s %s: %s"
      (Loc.to_string @@ get_loc lexbuf)
      __MODULE__
      msg

let skip_line = Lexing.new_line

let skip_char lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_cnum = pos.pos_cnum + 1 }

}

let whitespace = [' ' '\009' '\012']
let newline = "\r" ? "\n"
let id_char =  ['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']
let lower_id = ['a'-'z'] id_char*
let upper_id = ['A'-'Z'] id_char*

rule prog = parse
  | whitespace                        { skip_char lexbuf; prog lexbuf }
  | newline                           { skip_line lexbuf; prog lexbuf }
  | "=>"                              { Parser.D_ARROW }
  | "->"                              { Parser.S_ARROW }
  | "*"                               { Parser.ASTERIKS }
  | "\\"                              { Parser.B_SLASH }
  | "|"                               { Parser.V_BAR }
  | "."                               { Parser.PERIOD }
  | ":"                               { Parser.COLON }
  | "::"                              { Parser.COL_COL }
  | ";"                               { Parser.SEMICOLON }
  | "="                               { Parser.EQ }
  | "("                               { Parser.O_PAREN }
  | ")"                               { Parser.C_PAREN }
  | "["                               { Parser.O_BRACK }
  | "]"                               { Parser.C_BRACK }
  | "{"                               { Parser.O_BRACE }
  | "}"                               { Parser.C_BRACE }
  | "case"                            { Parser.CASE }
  | "exists"                          { Parser.EXISTS }
  | "forall"                          { Parser.FOR_ALL }
  | "let"                             { Parser.LET }
  | "in"                              { Parser.IN }
  | "mu"                              { Parser.MU }
  | "of"                              { Parser.OF }
  | "rec"                             { Parser.REC }
  | lower_id as id                    { Parser.LOWER_ID id }
  | "'" (upper_id as id)              { Parser.TICK_UPPER_ID id }
  | upper_id as id                    { Parser.UPPER_ID id }
  | eof                               { Parser.EOF }
  | _ as c                            { error lexbuf @@
                                          Printf.sprintf
                                            "unexpected character '%c'"
                                            c }
