%{

module Id = Identifier
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

module Type = struct

  include Type

  let var id = var @@ Id.of_string id

end

module Term = struct

  include Term

  let var id = var ~loc:(get_loc ()) @@ Id.of_string id

  let abs arg tp body = abs ~loc:(get_loc ()) (Id.of_string arg) tp body

  let app fn arg = app ~loc:(get_loc ()) fn arg

end

%}

/* Literals and identifiers */
%token <string> LOWER_ID
%token <string> UPPER_ID

/* Symbols */
%token ASTERIKS
%token B_SLASH
%token S_ARROW
%token PERIOD
%token COLON
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
  | UPPER_ID EQ typo              { Command.bind_type $1 $3 }
  | LOWER_ID EQ term              { Command.bind_term $1 $3 }
  | term                          { Command.eval_term $1 }

typo :
  | comp_typo                     { $1 }

comp_typo :
  | atom_typo                     { $1 }
  | atom_typo S_ARROW comp_typo   { Type.func $1 $3 }

atom_typo :
  | O_PAREN typo C_PAREN          { $2 }
  | O_PAREN typo error            { error "unclosed parenthesis" }
  | ASTERIKS                      { Type.base }
  | UPPER_ID                      { Type.var $1 }

term :
  | comp_term                     { $1 }
  | B_SLASH LOWER_ID COLON typo PERIOD term   { Term.abs $2 $4 $6 }

comp_term :
  | atom_term                     { $1 }
  | comp_term atom_term           { Term.app $1 $2 }

atom_term :
  | O_PAREN term C_PAREN          { $2 }
  | O_PAREN term error            { error "unclosed parenthesis" }
  | LOWER_ID                      { Term.var $1 }
