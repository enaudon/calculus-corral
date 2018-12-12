(**
  [parse_argv argv] parses the command-line flags in [argv], and returns
  a pair containing the path to the specified REPL executable, and the
  path to the directory of test cases.
 *)
val parse_argv : string array -> string * string

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
