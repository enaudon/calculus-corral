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

module Term = struct

  include Term

  let var id = var ~loc:(get_loc ()) @@ Id.define id

  let abs arg body = abs ~loc:(get_loc ()) (Id.define arg) body

  let app fn arg = app ~loc:(get_loc ()) fn arg

end

%}

/* Literals and identifiers */
%token <string> LOWER_ID

/* Symbols */
%token B_SLASH
%token PERIOD
%token SEMICOLON
%token EQ
%token O_PAREN C_PAREN

/* Other */
%token EOF

%start term
%start commands
%type < Term.t > term
%type < (Type.t, Term.t) Command.t list > commands

%%

/* Commands */

commands :
  | EOF                           { [] }
  | command SEMICOLON commands    { $1 :: $3 }

command :
  | LOWER_ID EQ term              { Command.bind_term $1 $3 }
  | term                          { Command.eval_term $1 }

/* Terms */

term :
  | comp_term                     { $1 }
  | B_SLASH LOWER_ID PERIOD term  { Term.abs $2 $4 }

comp_term :
  | atom_term                     { $1 }
  | comp_term atom_term           { Term.app $1 $2 }

atom_term :
  | O_PAREN term C_PAREN          { $2 }
  | O_PAREN term error            { error "unclosed parenthesis" }
  | LOWER_ID                      { Term.var $1 }
