%{

module Loc = Location

let get_loc () =
  Loc.of_lex_pos (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ())

let error msg =
  failwith @@ Printf.sprintf "%s %s" (Loc.to_string @@ get_loc ()) msg

let var id = Term.var ~loc:(get_loc ()) id

let abs id tp arg = Term.abs ~loc:(get_loc ()) id tp arg

let app fn arg = Term.app ~loc:(get_loc ()) fn arg

%}

/* Literals and identifiers */
%token <string> LOWER_ID

/* Keywords */
%token UPPER_B

/* Symbols */
%token COLON
%token ARROW
%token PERIOD
%token SEMICOLON
%token O_PAREN C_PAREN

/* Other */
%token EOF

%start term
%type < Term.t > term
%type < Type.t > typo

%%

typo :
  | comp_typo                     { $1 }

comp_typo :
  | atom_typo ARROW comp_typo     { Type.func $1 $3 }
  | atom_typo                     { $1 }

atom_typo :
  | O_PAREN typo C_PAREN          { $2 }
  | O_PAREN typo error            { error "Parser: unclosed parenthesis" }
  | UPPER_B                       { Type.base }

term :
  | comp_term                     { $1 }
  | LOWER_ID COLON typo PERIOD term   { abs $1 $3 $5 }

comp_term :
  | atom_term                     { $1 }
  | comp_term atom_term           { app $1 $2 }

atom_term :
  | O_PAREN term C_PAREN          { $2 }
  | O_PAREN term error            { error "Parser: unclosed parenthesis" }
  | LOWER_ID                      { var $1 }
