%{

module Loc = Location

let get_loc () =
  Loc.of_lex_pos
    (Parsing.symbol_start_pos ())
    (Parsing.symbol_end_pos ())

let error msg =
  failwith @@
    Printf.sprintf "%s %s: %s"
      (Loc.to_string @@ get_loc ())
      __MODULE__
      msg

let var id = Term.var ~loc:(get_loc ()) id

let abs id tp arg = Term.abs ~loc:(get_loc ()) id tp arg

let app fn arg = Term.app ~loc:(get_loc ()) fn arg

%}

/* Literals and identifiers */
%token <string> LOWER_ID
%token <string> UPPER_ID

/* Symbols */
%token ASTERIKS
%token B_SLASH
%token S_ARROW D_ARROW
%token PERIOD
%token COLON COL_COL
%token SEMICOLON
%token EQ
%token O_PAREN C_PAREN

/* Other */
%token EOF

%start typo
%start term
%start commands
%type < Type.t > typo
%type < Term.t > term
%type < (Type.t, Term.t) Command.t list > commands

%%

commands :
  | /* empty */                   { [] }
  | command SEMICOLON commands    { $1 :: $3 }

command :
  | LOWER_ID EQ term              { Command.bind_term $1 $3 }
  | term                          { Command.eval_term $1 }

kind :
  | comp_kind                     { $1 }

comp_kind :
  | atom_kind                     { $1 }
  | atom_kind D_ARROW comp_kind   { Kind.func $1 $3 }

atom_kind :
  | O_PAREN kind C_PAREN          { $2 }
  | O_PAREN kind error            { error "unclosed parenthesis" }
  | ASTERIKS                      { Kind.base }

typo :
  | arrow_typo                    { $1 }
  | B_SLASH UPPER_ID COL_COL kind PERIOD typo   { Type.abs $2 $4 $6 }

arrow_typo :
  | app_typo                      { $1 }
  | atom_typo S_ARROW arrow_typo  { Type.func $1 $3 }

app_typo :
  | atom_typo                     { $1 }
  | app_typo atom_typo            { Type.app $1 $2 }

atom_typo :
  | O_PAREN typo C_PAREN          { $2 }
  | O_PAREN typo error            { error "closed parenthesis" }
  | ASTERIKS                      { Type.base }
  | UPPER_ID                      { Type.var $1 }

term :
  | comp_term                     { $1 }
  | B_SLASH LOWER_ID COLON typo PERIOD term   { abs $2 $4 $6 }

comp_term :
  | atom_term                     { $1 }
  | comp_term atom_term           { app $1 $2 }

atom_term :
  | O_PAREN term C_PAREN          { $2 }
  | O_PAREN term error            { error "closed parenthesis" }
  | LOWER_ID                      { var $1 }
