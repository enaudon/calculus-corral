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

  let var id = var @@ Id.define id

  let abs arg kn body = abs (Id.define arg) kn body

  let forall quant kn body = forall (Id.define quant) kn body

end

module Term = struct

  include Term

  let var id = var ~loc:(get_loc ()) @@ Id.define id

  let abs arg tp body = abs ~loc:(get_loc ()) (Id.define arg) tp body

  let app fn arg = app ~loc:(get_loc ()) fn arg

  let tp_abs arg body = tp_abs ~loc:(get_loc ()) (Id.define arg) body

  let tp_app fn arg = tp_app ~loc:(get_loc ()) fn arg

end

%}

/* Literals and identifiers */
%token <string> LOWER_ID
%token <string> UPPER_ID

/* Keywords */
%token FOR_ALL

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

/* Commands */

commands :
  | EOF                           { [] }
  | command SEMICOLON commands    { $1 :: $3 }

command :
  | UPPER_ID EQ typo              { Command.bind_type $1 $3 }
  | LOWER_ID EQ term              { Command.bind_term $1 $3 }
  | term                          { Command.eval_term $1 }

/* Kinds */

kind :
  | comp_kind                     { $1 }

comp_kind :
  | atom_kind                     { $1 }
  | atom_kind D_ARROW comp_kind   { Kind.oper $1 $3 }

atom_kind :
  | O_PAREN kind C_PAREN          { $2 }
  | O_PAREN kind error            { error "unclosed parenthesis" }
  | ASTERIKS                      { Kind.prop }

/* Types */

typo :
  | arrow_typo                    { $1 }
  | B_SLASH UPPER_ID COL_COL kind PERIOD typo   { Type.abs $2 $4 $6 }
  | FOR_ALL UPPER_ID COL_COL kind PERIOD typo   { Type.forall $2 $4 $6 }

arrow_typo :
  | app_typo                      { $1 }
  | app_typo S_ARROW arrow_typo   { Type.func $1 $3 }

app_typo :
  | atom_typo                     { $1 }
  | app_typo atom_typo            { Type.app $1 $2 }

atom_typo :
  | O_PAREN typo C_PAREN          { $2 }
  | O_PAREN typo error            { error "unclosed parenthesis" }
  | UPPER_ID                      { Type.var $1 }

/* Terms */

term :
  | comp_term                     { $1 }
  | B_SLASH UPPER_ID COL_COL kind PERIOD term   { Term.tp_abs $2 $4 $6 }
  | B_SLASH LOWER_ID COLON typo PERIOD term   { Term.abs $2 $4 $6 }

comp_term :
  | atom_term                     { $1 }
  | comp_term atom_typo           { Term.tp_app $1 $2 }
  | comp_term atom_term           { Term.app $1 $2 }

atom_term :
  | O_PAREN term C_PAREN          { $2 }
  | O_PAREN term error            { error "unclosed parenthesis" }
  | LOWER_ID                      { Term.var $1 }
