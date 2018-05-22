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

let pack tp1 tm tp2 = Term.pack ~loc:(get_loc ()) tp1 tm tp2

let unpack tp_id tm_id pack body =
  Term.unpack ~loc:(get_loc ()) tp_id tm_id pack body

%} 
/* Literals and identifiers */
%token <string> LOWER_ID
%token <string> UPPER_ID

/* Keywords */
%token AS
%token EXISTS
%token IN
%token PACK
%token UNPACK

/* Symbols */
%token ASTERIKS
%token B_SLASH
%token S_ARROW
%token PERIOD
%token COMMA
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
  | EXISTS UPPER_ID PERIOD typo   { Type.exists $2 $4 }
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
  | B_SLASH LOWER_ID COLON typo PERIOD term   { abs $2 $4 $6 }
  | PACK typo COMMA term AS typo  { pack $2 $4 $6 }
  | UNPACK UPPER_ID COMMA LOWER_ID EQ term IN term  { unpack $2 $4 $6 $8 }

comp_term :
  | atom_term                     { $1 }
  | comp_term atom_term           { app $1 $2 }

atom_term :
  | O_PAREN term C_PAREN          { $2 }
  | O_PAREN term error            { error "unclosed parenthesis" }
  | LOWER_ID                      { var $1 }
