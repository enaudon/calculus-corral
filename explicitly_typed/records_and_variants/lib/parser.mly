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

  let forall quants body = forall (Id.of_string quants) body

  let rcrd fields =
    rcrd @@ List.map (fun (id, tp) -> Id.of_string id, tp) fields

  let vrnt fields =
    let fn (id, tp) = match tp with
      | None -> Id.of_string id, rcrd []
      | Some tp -> Id.of_string id, tp
    in
    vrnt @@ List.map fn fields

end

module Term = struct

  include Term

  let var id = var ~loc:(get_loc ()) @@ Id.of_string id

  let abs arg tp body = abs ~loc:(get_loc ()) (Id.of_string arg) tp body

  let app fn arg = app ~loc:(get_loc ()) fn arg

  let tp_abs arg body = tp_abs ~loc:(get_loc ()) (Id.of_string arg) body

  let tp_app fn arg = tp_app ~loc:(get_loc ()) fn arg

  let rcrd fields =
    rcrd ~loc:(get_loc ()) @@
      List.map (fun (id, tm) -> Id.of_string id, tm) fields

  let proj rcrd field =
    proj ~loc:(get_loc ()) rcrd @@ Id.of_string field

  let vrnt case data tp = match data with
    | None -> vrnt ~loc:(get_loc ()) (Id.of_string case) (rcrd []) tp
    | Some data -> vrnt ~loc:(get_loc ()) (Id.of_string case) data tp

  let case vrnt cases =
    let fn (case, id, tm) = match id with
      | None -> Id.of_string case, Id.of_string "_", tm
      | Some id ->  Id.of_string case, Id.of_string id, tm
    in
    case ~loc:(get_loc ()) vrnt @@ List.map fn cases

end
%}

/* Literals and identifiers */
%token <string> LOWER_ID
%token <string> UPPER_ID

/* Keywords */
%token CASE
%token FOR_ALL
%token OF

/* Symbols */
%token B_SLASH
%token S_ARROW
%token PERIOD
%token COLON
%token SEMICOLON
%token EQ
%token O_PAREN C_PAREN
%token O_BRACK C_BRACK
%token O_BRACE C_BRACE

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
  | FOR_ALL UPPER_ID PERIOD typo  { Type.forall $2 $4 }

comp_typo :
  | atom_typo                     { $1 }
  | atom_typo S_ARROW comp_typo   { Type.func $1 $3 }

atom_typo :
  | O_PAREN typo C_PAREN          { $2 }
  | O_PAREN typo error            { error "unclosed parenthesis" }
  | O_BRACE field_list_typo C_BRACE   { Type.rcrd $2 }
  | O_BRACE typo error            { error "unclosed brace" }
  | O_BRACK case_list_typo C_BRACK  { Type.vrnt $2 }
  | O_BRACK typo error            { error "unclosed bracket" }
  | UPPER_ID                      { Type.var $1 }

field_list_typo :
  | LOWER_ID COLON typo           { [($1, $3)] }
  | LOWER_ID COLON typo SEMICOLON   { [($1, $3)] }
  | LOWER_ID COLON typo SEMICOLON field_list_typo   { ($1, $3) :: $5 }

case_list_typo :
  | UPPER_ID COLON typo           { [($1, Some $3)] }
  | UPPER_ID COLON typo SEMICOLON   { [($1, Some $3)] }
  | UPPER_ID COLON typo SEMICOLON case_list_typo
    { ($1, Some $3) :: $5 }
  | UPPER_ID                      { [($1, None)] }
  | UPPER_ID SEMICOLON            { [($1, None)] }
  | UPPER_ID SEMICOLON case_list_typo   { ($1, None) :: $3 }

term :
  | comp_term                     { $1 }
  | B_SLASH UPPER_ID PERIOD term  { Term.tp_abs $2 $4 }
  | B_SLASH LOWER_ID COLON typo PERIOD term   { Term.abs $2 $4 $6 }
  | CASE term OF O_BRACK case_list_term C_BRACK   { Term.case $2 $5 }

comp_term :
  | atom_term                     { $1 }
  | comp_term atom_term           { Term.app $1 $2 }
  | comp_term atom_typo           { Term.tp_app $1 $2 }

atom_term :
  | O_PAREN term C_PAREN          { $2 }
  | O_PAREN term error            { error "unclosed parenthesis" }
  | O_BRACE field_list_term C_BRACE   { Term.rcrd $2 }
  | O_BRACE term error            { error "unclosed brace" }
  | atom_term PERIOD LOWER_ID     { Term.proj $1 $3 }
  | O_BRACK UPPER_ID term OF typo C_BRACK
    { Term.vrnt $2 (Some $3) $5 }
  | O_BRACK UPPER_ID OF typo C_BRACK  { Term.vrnt $2 None $4 }
  | O_BRACK term error            { error "unclosed bracket" }
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
