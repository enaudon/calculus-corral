(**
  [make_implicit repl_exe case_dir cases] creates a test suite for an
  implicitly-typed REPL.
 *)
val make_implicit :
  string -> string -> (string * string list) list -> OUnit.test

(**
  [make_explicit repl_exe case_dir cases] creates a test suite for an
  explicitly-typed REPL.
 *)
val make_explicit :
  string -> string -> (string * string list) list -> OUnit.test
