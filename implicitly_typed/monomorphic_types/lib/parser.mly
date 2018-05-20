%{

module Loc = Location

let get_loc () =
  Loc.of_lex_pos
    (Parsing.symbol_start_pos ())
    (Parsing.symbol_end_pos ())

let error msg =
  failwith @@ Printf.sprintf "%s %s" (Loc.to_string @@ get_loc ()) msg

let var id = Term.var ~loc:(get_loc ()) id

let abs arg body = Term.abs ~loc:(get_loc ()) arg body

let app fn arg = Term.app ~loc:(get_loc ()) fn arg

%}

/* Literals and identifiers */
%token <string> LOWER_ID

/* Symbols */
%token COLON
%token ARROW
%token PERIOD
%token SEMICOLON
%token O_PAREN C_PAREN

%start term
%type < Term.t > term

%%

term :
  | comp_term                     { $1 }
  | LOWER_ID PERIOD term          { abs $1 $3 }

comp_term :
  | atom_term                     { $1 }
  | comp_term atom_term           { app $1 $2 }

atom_term :
  | O_PAREN term C_PAREN          { $2 }
  | O_PAREN term error            { error "Parsing error -- unclosed parenthesis" }
  | LOWER_ID                      { var $1 }
