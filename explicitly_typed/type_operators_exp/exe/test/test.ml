open OUnit

let _ =
  let repl_exe, case_dir = Test_repl.parse_argv Sys.argv in
  let repl_cases =
    [ ("function.lc", []);
      ("church_boolean.lc", []);
      ("church_natural.lc", []);
      ("church_pair.lc", ["church_boolean.lc"; "church_natural.lc"]) ]
  in
  let test_suite =
    "explicitly_typed::type_operators"
    >::: [ Test_term.make ();
           Test_repl.make_explicit repl_exe case_dir repl_cases ]
  in
  run_test_tt_main test_suite
