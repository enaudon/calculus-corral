type t = {
  file : string ;
  start : int * int ;
  finish : int * int ;
}

let dummy = { file = ""; start = 0, -1; finish = 0, -1 }

(*
  Before creating the location, [of_lex_pos] ensures that the end [e] is
  greater than the start, [s]. 
 *)
let of_lex_pos s e =
  let open Lexing in
  if s.pos_fname <> e.pos_fname then
    failwith @@
      Printf.sprintf "Location.of_lex_pos: different file names";
  if s.pos_lnum > e.pos_lnum then
    failwith @@
      Printf.sprintf "Location.of_lex_pos: start line > end line";
  if s.pos_lnum = e.pos_lnum && s.pos_cnum > e.pos_cnum then
    failwith @@
      Printf.sprintf "Location.of_lex_pos: start char > end char";
  {
    file = s.pos_fname ;
    start = s.pos_lnum, s.pos_cnum - s.pos_bol ;
    finish = e.pos_lnum, e.pos_cnum - e.pos_bol ;
  }

let to_string loc =
  let { file; start = s_line, s_char; finish = f_line, f_char } = loc in
  let file = if file = "" then "<unknown file>" else file in
  if loc = dummy then
    Printf.sprintf "%s (<unknown line/char>)" file
  else if s_line = f_line && s_char = f_char then
    Printf.sprintf "%s (%d.%d)" file s_line s_char
  else if s_line = f_line then
    Printf.sprintf "%s (%d.%d-%d)" file s_line s_char f_char
  else
    Printf.sprintf "%s (%d.%d - %d.%d)" file s_line s_char f_line f_char
