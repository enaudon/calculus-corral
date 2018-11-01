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

  let bind id value body =
    bind ~loc:(get_loc ()) (Id.define id) value body

  let rcrd fields =
    rcrd ~loc:(get_loc ()) @@
      List.map (fun (id, tm) -> Id.define id, tm) fields

  let proj rcrd field =
    proj ~loc:(get_loc ()) rcrd @@ Id.define field

  let vrnt case data = match data with
    | None -> vrnt ~loc:(get_loc ()) (Id.define case) (rcrd [])
    | Some data -> vrnt ~loc:(get_loc ()) (Id.define case) data

  let case vrnt cases =
    let fn (case, id, tm) = match id with
      | None -> Id.define case, Id.define "_", tm
      | Some id -> Id.define case, Id.define id, tm
    in
    case ~loc:(get_loc ()) vrnt @@ List.map fn cases

end

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
  | B_SLASH LOWER_ID PERIOD term  { Term.abs $2 $4 }
  | LET LOWER_ID EQ term IN term  { Term.bind $2 $4 $6 }
  | UPPER_ID term                 { Term.vrnt $1 (Some $2) }
  | UPPER_ID                      { Term.vrnt $1 None }
  | CASE term OF O_BRACK case_list_term C_BRACK   { Term.case $2 $5 }

comp_term :
  | atom_term                     { $1 }
  | comp_term atom_term           { Term.app $1 $2 }

atom_term :
  | O_PAREN term C_PAREN          { $2 }
  | O_PAREN term error            { error "unclosed parenthesis" }
  | O_BRACE field_list_term C_BRACE   { Term.rcrd $2 }
  | O_BRACE field_list_term error   { error "unclosed brace" }
  | atom_term PERIOD LOWER_ID     { Term.proj $1 $3 }
  | LOWER_ID                      { Term.var $1 }

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
