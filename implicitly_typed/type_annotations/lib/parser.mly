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

  let inf_var id = inf_var @@ Id.define id

end

module Annot = struct

  include Type_annotation

  let forall quants body = forall (List.map Id.define quants) body

  let exists quants body = exists (List.map Id.define quants) body

end

module Term = struct

  include Term

  let var id = var ~loc:(get_loc ()) @@ Id.define id

  let abs arg body = abs ~loc:(get_loc ()) (Id.define arg) body

  let app fn arg = app ~loc:(get_loc ()) fn arg

  let bind id value body =
    bind ~loc:(get_loc ()) (Id.define id) value body

  let annot tm tp = annot ~loc:(get_loc ()) tm tp

end

%}

/* Literals and identifiers */
%token <string> LOWER_ID
%token <string> TICK_UPPER_ID

/* Keywords */
%token EXISTS
%token FOR_ALL
%token LET IN

/* Symbols */
%token B_SLASH
%token S_ARROW
%token PERIOD
%token COLON
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

/* Types */

typo :
  | comp_typo                     { $1 }

comp_typo :
  | atom_typo                     { $1 }
  | atom_typo S_ARROW comp_typo   { Type.func $1 $3 }

atom_typo :
  | O_PAREN typo C_PAREN          { $2 }
  | O_PAREN typo error            { error "unclosed parenthesis" }
  | TICK_UPPER_ID                 { Type.inf_var $1 }

annot :
  | FOR_ALL tick_upper_id_list PERIOD typo  { Annot.forall $2 $4 }
  | EXISTS tick_upper_id_list PERIOD typo   { Annot.exists $2 $4 }
  | typo                          { Annot.typo $1 }

tick_upper_id_list :
  | TICK_UPPER_ID                 { [$1] }
  | TICK_UPPER_ID tick_upper_id_list  { $1 :: $2 }

/* Terms */

term :
  | annot_term                    { $1 }
  | B_SLASH LOWER_ID PERIOD term  { Term.abs $2 $4 }
  | LET LOWER_ID EQ term IN term  { Term.bind $2 $4 $6 }

annot_term :
  | comp_term                     { $1 }
  | annot_term COLON annot        { Term.annot $1 $3 }

comp_term :
  | atom_term                     { $1 }
  | comp_term atom_term           { Term.app $1 $2 }

atom_term :
  | O_PAREN term C_PAREN          { $2 }
  | O_PAREN term error            { error "unclosed parenthesis" }
  | LOWER_ID                      { Term.var $1 }
