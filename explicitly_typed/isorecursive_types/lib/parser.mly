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

  let mu quant kn body = mu (Id.define quant) kn body

  let rcrd (fields, rest) =
    let fn (id, tp) = Id.define id, tp in
    rcrd (List.map fn fields) rest

  let vrnt (cases, rest) =
    let fn (id, tp) = match tp with
      | None -> Id.define id, rcrd ([], None)
      | Some tp -> Id.define id, tp
    in
    vrnt (List.map fn cases) rest

  let rcrd_row (fields, rest) =
    let fn (id, tp) = Id.define id, tp in
    row (List.map fn fields) rest

  let vrnt_row (cases, rest) =
    let fn (id, tp) = match tp with
      | None -> Id.define id, rcrd ([], None)
      | Some tp -> Id.define id, tp
    in
    row (List.map fn cases) rest

end

module Term = struct

  include Term

  let var id = var ~loc:(get_loc ()) @@ Id.define id

  let abs arg tp body = abs ~loc:(get_loc ()) (Id.define arg) tp body

  let app fn arg = app ~loc:(get_loc ()) fn arg

  let tp_abs arg body = tp_abs ~loc:(get_loc ()) (Id.define arg) body

  let tp_app fn arg = tp_app ~loc:(get_loc ()) fn arg

  let roll tp tm = roll ~loc:(get_loc ()) tp tm

  let unroll tm = unroll ~loc:(get_loc ()) tm

  let fix tm = fix ~loc:(get_loc ()) tm

  let rcrd fields =
    rcrd ~loc:(get_loc ()) @@
      List.map (fun (id, tm) -> Id.define id, tm) fields

  let proj rcrd field =
    proj ~loc:(get_loc ()) rcrd @@ Id.define field

  let vrnt (case, data, tp) = match data with
    | None -> vrnt ~loc:(get_loc ()) (Id.define case) (rcrd []) tp
    | Some data -> vrnt ~loc:(get_loc ()) (Id.define case) data tp

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
%token FIX
%token FOR_ALL
%token MU
%token ROLL
%token UNROLL
%token OF

/* Symbols */
%token ASTERIKS
%token B_SLASH
%token V_BAR
%token S_ARROW D_ARROW
%token PERIOD
%token COLON COL_COL
%token SEMICOLON
%token EQ
%token O_PAREN C_PAREN
%token O_BRACK C_BRACK
%token O_BRACE C_BRACE
%token O_CHEVR C_CHEVR

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
  | O_CHEVR C_CHEVR               { Kind.row }

/* Types */

typo :
  | arrow_typo                    { $1 }
  | B_SLASH UPPER_ID COL_COL kind PERIOD typo   { Type.abs $2 $4 $6 }
  | FOR_ALL UPPER_ID COL_COL kind PERIOD typo   { Type.forall $2 $4 $6 }
  | MU UPPER_ID COL_COL kind PERIOD typo  { Type.mu $2 $4 $6 }

arrow_typo :
  | app_typo                      { $1 }
  | app_typo S_ARROW arrow_typo   { Type.func $1 $3 }

app_typo :
  | atom_typo                     { $1 }
  | app_typo atom_typo            { Type.app $1 $2 }

atom_typo :
  | O_PAREN typo C_PAREN          { $2 }
  | O_PAREN typo error            { error "unclosed parenthesis" }
  | O_BRACE rcrd_typo C_BRACE     { Type.rcrd $2 }
  | O_BRACE rcrd_typo error       { error "unclosed brace" }
  | O_BRACK vrnt_typo C_BRACK     { Type.vrnt $2 }
  | O_BRACK vrnt_typo error       { error "unclosed bracket" }
  | O_CHEVR rcrd_typo C_CHEVR     { Type.rcrd_row $2 }
  | O_CHEVR vrnt_typo C_CHEVR     { Type.vrnt_row $2 }
  | O_CHEVR C_CHEVR               { Type.rcrd_row ([], None) }
  | UPPER_ID                      { Type.var $1 }

rcrd_typo :
  | field_list_typo               { ($1, None) }
  | field_list_typo V_BAR typo    { ($1, Some $3) }

field_list_typo :
  | LOWER_ID COLON typo           { [($1, $3)] }
  | LOWER_ID COLON typo SEMICOLON   { [($1, $3)] }
  | LOWER_ID COLON typo SEMICOLON field_list_typo   { ($1, $3) :: $5 }

vrnt_typo :
  | case_list_typo                { ($1, None) }
  | case_list_typo V_BAR typo     { ($1, Some $3) }

case_list_typo :
  | UPPER_ID COLON typo           { [($1, Some $3)] }
  | UPPER_ID COLON typo SEMICOLON   { [($1, Some $3)] }
  | UPPER_ID COLON typo SEMICOLON case_list_typo
    { ($1, Some $3) :: $5 }
  | UPPER_ID                      { [($1, None)] }
  | UPPER_ID SEMICOLON            { [($1, None)] }
  | UPPER_ID SEMICOLON case_list_typo   { ($1, None) :: $3 }

/* Terms */

term :
  | comp_term                     { $1 }
  | B_SLASH UPPER_ID COL_COL kind PERIOD term   { Term.tp_abs $2 $4 $6 }
  | B_SLASH LOWER_ID COLON typo PERIOD term   { Term.abs $2 $4 $6 }
  | CASE term OF O_BRACK case_list_term C_BRACK   { Term.case $2 $5 }
  | ROLL atom_typo comp_term      { Term.roll $2 $3 }
  | UNROLL comp_term              { Term.unroll $2 }
  | FIX comp_term                 { Term.fix $2 }

comp_term :
  | atom_term                     { $1 }
  | comp_term atom_typo           { Term.tp_app $1 $2 }
  | comp_term atom_term           { Term.app $1 $2 }

atom_term :
  | O_PAREN term C_PAREN          { $2 }
  | O_PAREN term error            { error "unclosed parenthesis" }
  | O_BRACE field_list_term C_BRACE   { Term.rcrd $2 }
  | O_BRACE field_list_term error   { error "unclosed brace" }
  | atom_term PERIOD LOWER_ID     { Term.proj $1 $3 }
  | O_BRACK vrnt_term C_BRACK     { Term.vrnt $2 }
  | O_BRACK vrnt_term error       { error "unclosed bracket" }
  | LOWER_ID                      { Term.var $1 }

field_list_term :
  | LOWER_ID EQ term              { [($1, $3)] }
  | LOWER_ID EQ term SEMICOLON    { [($1, $3)] }
  | LOWER_ID EQ term SEMICOLON field_list_term  { ($1, $3) :: $5 }

vrnt_term :
  | UPPER_ID OF typo              { ($1, None, $3) }
  | UPPER_ID term OF typo         { ($1, Some $2, $4) }

case_list_term :
  | UPPER_ID LOWER_ID S_ARROW term  { [($1, Some $2, $4)] }
  | UPPER_ID LOWER_ID S_ARROW term SEMICOLON  { [($1, Some $2, $4)] }
  | UPPER_ID LOWER_ID S_ARROW term SEMICOLON case_list_term
    { ($1, Some $2, $4) :: $6 }
  | UPPER_ID S_ARROW term         { [($1, None, $3)] }
  | UPPER_ID S_ARROW term SEMICOLON   { [($1, None, $3)] }
  | UPPER_ID S_ARROW term SEMICOLON case_list_term
    { ($1, None, $3) :: $5 }
