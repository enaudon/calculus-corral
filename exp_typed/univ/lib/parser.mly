%{

module Loc = Location

let get_loc () =
  Loc.of_lex_pos (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ())

let error msg =
  failwith @@ Printf.sprintf "%s %s" (Loc.to_string @@ get_loc ()) msg

let var id = Term.var ~loc:(get_loc ()) id

let abs id tp arg = Term.abs ~loc:(get_loc ()) id tp arg

let app fn arg = Term.app ~loc:(get_loc ()) fn arg

let tp_abs id arg = Term.tp_abs ~loc:(get_loc ()) id arg

let tp_app fn arg = Term.tp_app ~loc:(get_loc ()) fn arg

%}

/* Literals and identifiers */
%token <string> LOWER_ID
%token <string> UPPER_ID

/* Keywords */
%token FOR_ALL

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
  | FOR_ALL UPPER_ID PERIOD typo  { Type.forall $2 $4 }
  | comp_typo                     { $1 }

comp_typo :
  | atom_typo ARROW comp_typo     { Type.func $1 $3 }
  | atom_typo                     { $1 }

atom_typo :
  | O_PAREN typo C_PAREN          { $2 }
  | O_PAREN typo error            { error "Parser: unclosed parenthesis" }
  | UPPER_ID                      { Type.var $1 }

term :
  | comp_term                     { $1 }
  | LOWER_ID COLON typo PERIOD term   { abs $1 $3 $5 }
  | UPPER_ID PERIOD term          { tp_abs $1 $3 }

comp_term :
  | atom_term                     { $1 }
  | comp_term atom_term           { app $1 $2 }
  | comp_term atom_typo           { tp_app $1 $2 }

atom_term :
  | O_PAREN term C_PAREN          { $2 }
  | O_PAREN term error            { error "Parser: unclosed parenthesis" }
  | LOWER_ID                      { var $1 }
