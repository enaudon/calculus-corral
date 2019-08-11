%{

module Id = Identifier
module Loc = Location
module Opt = Option

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

  let var id = var @@ Id.define id

  let abs arg kn body = abs (Id.define arg) kn body

  let mu quant kn body = mu (Id.define quant) kn body

  let rcrd (fields, rest) =
    let fn (id, tp) = Id.define id, tp in
    rcrd (List.map fn fields) rest

  let vrnt (cases, rest) =
    let fn (id, tp) = match tp with
      | None -> Id.define id, rcrd ([], Opt.none)
      | Some tp -> Id.define id, tp
    in
    vrnt (List.map fn cases) rest

end

module Annot = struct

  include Type_annotation

  let forall quant kn body = forall (Id.define quant) kn body

  let exists quant kn body = exists (Id.define quant) kn body

end

module Term = struct

  include Term

  let var id = var ~loc:(get_loc ()) @@ Id.define id

  let abs arg body = abs ~loc:(get_loc ()) (Id.define arg) body

  let app fn arg = app ~loc:(get_loc ()) fn arg

  let bind ?recf id value body =
    bind ~loc:(get_loc ()) ?recf (Id.define id) value body

  let annot tm tp = annot ~loc:(get_loc ()) tm tp

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
%token <string> TICK_UPPER_ID

/* Keywords */
%token EXISTS
%token FOR_ALL
%token MU
%token CASE OF
%token LET REC IN

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

annot :
  | FOR_ALL TICK_UPPER_ID COL_COL kind PERIOD annot
    { Annot.forall $2 $4 $6 }
  | EXISTS TICK_UPPER_ID COL_COL kind PERIOD annot
    { Annot.exists $2 $4 $6 }
  | typo
    { Annot.typo $1 }

typo :
  | arrow_typo                    { $1 }
  | B_SLASH UPPER_ID COL_COL kind PERIOD typo   { Type.abs $2 $4 $6 }
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
  | TICK_UPPER_ID                 { Type.inf_var $1 }
  | UPPER_ID                      { Type.var $1 }

rcrd_typo :
  | field_list_typo               { ($1, Opt.none) }
  | field_list_typo V_BAR typo    { ($1, Opt.some $3) }

field_list_typo :
  | LOWER_ID COLON typo           { [($1, $3)] }
  | LOWER_ID COLON typo SEMICOLON   { [($1, $3)] }
  | LOWER_ID COLON typo SEMICOLON field_list_typo   { ($1, $3) :: $5 }

vrnt_typo :
  | case_list_typo                { ($1, Opt.none) }
  | case_list_typo V_BAR typo     { ($1, Opt.some $3) }

case_list_typo :
  | UPPER_ID COLON typo           { [($1, Opt.some $3)] }
  | UPPER_ID COLON typo SEMICOLON   { [($1, Opt.some $3)] }
  | UPPER_ID COLON typo SEMICOLON case_list_typo
    { ($1, Opt.some $3) :: $5 }
  | UPPER_ID                      { [($1, Opt.none)] }
  | UPPER_ID SEMICOLON            { [($1, Opt.none)] }
  | UPPER_ID SEMICOLON case_list_typo   { ($1, Opt.none) :: $3 }

/* Terms */

term :
  | annot_term                    { $1 }
  | B_SLASH LOWER_ID PERIOD term  { Term.abs $2 $4 }
  | LET LOWER_ID EQ term IN term  { Term.bind $2 $4 $6 }
  | LET REC LOWER_ID EQ term IN term  { Term.bind ~recf:() $3 $5 $7 }
  | UPPER_ID term                 { Term.vrnt $1 (Opt.some $2) }
  | UPPER_ID                      { Term.vrnt $1 Opt.none }
  | CASE term OF O_BRACK case_list_term C_BRACK   { Term.case $2 $5 }

annot_term :
  | comp_term                     { $1 }
  | annot_term COLON annot        { Term.annot $1 $3 }

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
  | UPPER_ID LOWER_ID S_ARROW term  { [($1, Opt.some $2, $4)] }
  | UPPER_ID LOWER_ID S_ARROW term SEMICOLON
    { [($1, Opt.some $2, $4)] }
  | UPPER_ID LOWER_ID S_ARROW term SEMICOLON case_list_term
    { ($1, Opt.some $2, $4) :: $6 }
  | UPPER_ID S_ARROW term         { [($1, Opt.none, $3)] }
  | UPPER_ID S_ARROW term SEMICOLON   { [($1, Opt.none, $3)] }
  | UPPER_ID S_ARROW term SEMICOLON case_list_term
    { ($1, Opt.none, $3) :: $5 }
