open OUnit

let _ =
  let repl_exe, case_dir = Test_repl.parse_argv Sys.argv in
  let repl_cases =
    [ ("function.lc", []);
      ("church_pair.lc", []);
      ("church_boolean.lc", ["church_pair.lc"]);
      ("church_natural.lc", ["church_pair.lc"]) ]
  in
  let test_suite =
    "implicitly_typed::monomorphic_types"
    >::: [Test_repl.make_implicit repl_exe case_dir repl_cases]
  in
  run_test_tt_main test_suite
