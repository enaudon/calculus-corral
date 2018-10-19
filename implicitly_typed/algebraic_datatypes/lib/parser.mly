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

let abs id arg = Term.abs ~loc:(get_loc ()) id arg

let app fn arg = Term.app ~loc:(get_loc ()) fn arg

let bind id value body = Term.bind ~loc:(get_loc ()) id value body

let rcrd fields = Term.rcrd ~loc:(get_loc ()) fields

let proj rcrd field = Term.proj ~loc:(get_loc ()) rcrd field

let vrnt case data = match data with
  | None -> Term.vrnt ~loc:(get_loc ()) case (Term.rcrd [])
  | Some data -> Term.vrnt ~loc:(get_loc ()) case data

let case vrnt cases =
  let fn (case, id, tm) = match id with
    | None -> case, "_", tm
    | Some id -> case, id, tm
  in
  Term.case ~loc:(get_loc ()) vrnt @@ List.map fn cases

%}

/* Literals and identifiers */
%token <string> LOWER_ID
%token <string> UPPER_ID

/* Keywords */
%token CASE
%token OF
%token LET IN

/* Symbols */
%token S_ARROW
%token B_SLASH
%token PERIOD
%token SEMICOLON
%token EQ
%token O_PAREN C_PAREN
%token O_BRACK C_BRACK
%token O_BRACE C_BRACE

/* Other */
%token EOF

%start term
%start commands
%type < Term.t > term
%type < (Type.t, Term.t) Command.t list > commands

%%

commands :
  | /* empty */                   { [] }
  | command SEMICOLON commands    { $1 :: $3 }

command :
  | LOWER_ID EQ term              { Command.bind_term $1 $3 }
  | term                          { Command.eval_term $1 }

term :
  | comp_term                     { $1 }
  | B_SLASH LOWER_ID PERIOD term  { abs $2 $4 }
  | LET LOWER_ID EQ term IN term  { bind $2 $4 $6 }
  | UPPER_ID term                 { vrnt $1 (Some $2) }
  | UPPER_ID                      { vrnt $1 None }
  | CASE term OF O_BRACK case_list_term C_BRACK   { case $2 $5 }

comp_term :
  | atom_term                     { $1 }
  | comp_term atom_term           { app $1 $2 }

atom_term :
  | O_PAREN term C_PAREN          { $2 }
  | O_PAREN term error            { error "unclosed parenthesis" }
  | O_BRACE field_list_term C_BRACE   { rcrd $2 }
  | O_BRACE field_list_term error   { error "unclosed brace" }
  | atom_term PERIOD LOWER_ID     { proj $1 $3 }
  | LOWER_ID                      { var $1 }

field_list_term :
  | LOWER_ID EQ term              { [($1, $3)] }
  | LOWER_ID EQ term SEMICOLON    { [($1, $3)] }
  | LOWER_ID EQ term SEMICOLON field_list_term  { ($1, $3) :: $5 }

case_list_term :
  | UPPER_ID LOWER_ID S_ARROW term  { [($1, Some $2, $4)] }
  | UPPER_ID LOWER_ID S_ARROW term SEMICOLON  { [($1, Some $2, $4)] }
  | UPPER_ID LOWER_ID S_ARROW term SEMICOLON case_list_term
    { ($1, Some $2, $4) :: $6 }
  | UPPER_ID S_ARROW term         { [($1, None, $3)] }
  | UPPER_ID S_ARROW term SEMICOLON   { [($1, None, $3)] }
  | UPPER_ID S_ARROW term SEMICOLON case_list_term
    { ($1, None, $3) :: $5 }
