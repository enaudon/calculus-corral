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
%token <string> UPPER_ID

/* Symbols */
%token B_SLASH
%token COLON
%token ARROW
%token PERIOD
%token O_PAREN C_PAREN

/* Other */
%token EOF

%start term
%type < Term.t > term
%type < Type.t > typo

%%

typo :
  | arrow_typo                    { $1 }
  | B_SLASH UPPER_ID PERIOD typo  { Type.abs $2 $4 }

arrow_typo :
  | app_typo                      { $1 }
  | atom_typo ARROW arrow_typo    { Type.func $1 $3 }

app_typo :
  | atom_typo                     { $1 }
  | app_typo atom_typo            { Type.app $1 $2 }

atom_typo :
  | O_PAREN typo C_PAREN          { $2 }
  | O_PAREN typo error            { error "Parser: unclosed parenthesis" }
  | UPPER_ID                      { Type.cst $1 }

term :
  | comp_term                     { $1 }
  | B_SLASH LOWER_ID COLON typo PERIOD term   { abs $2 $4 $6 }

comp_term :
  | atom_term                     { $1 }
  | comp_term atom_term           { app $1 $2 }

atom_term :
  | O_PAREN term C_PAREN          { $2 }
  | O_PAREN term error            { error "Parser: unclosed parenthesis" }
  | LOWER_ID                      { var $1 }
