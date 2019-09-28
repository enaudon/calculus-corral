open OUnit

let _ =
  let repl_exe, case_dir = Test_repl.parse_argv Sys.argv in
  let repl_cases =
    [ ("hotcw.lc", []);
      ("row_polymorphism.lc", []);
      ("function.lc", []);
      ("church_boolean.lc", []);
      ("church_natural.lc", []);
      ("church_pair.lc", ["church_boolean.lc"; "church_natural.lc"]);
      ("variant_boolean.lc", []);
      ("record_pair.lc", ["variant_boolean.lc"]);
      ("variant_option.lc", []) ]
  in
  let test_suite =
    "implicitly_typed::algebraic_types"
    >::: [Test_repl.make_implicit repl_exe case_dir repl_cases]
  in
  run_test_tt_main test_suite
