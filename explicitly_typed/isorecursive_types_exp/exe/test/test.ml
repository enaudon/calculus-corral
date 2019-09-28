open OUnit

let _ =
  let repl_exe, case_dir = Test_repl.parse_argv Sys.argv in
  let repl_cases =
    [ ("row_polymorphism.lc", []);
      ("function.lc", []);
      ("church_boolean.lc", []);
      ("church_natural.lc", []);
      ("church_pair.lc", ["church_boolean.lc"; "church_natural.lc"]);
      ("variant_boolean.lc", []);
      ("variant_natural.lc", []);
      ("record_pair.lc", ["variant_boolean.lc"; "variant_natural.lc"]);
      ("variant_option.lc", []);
      ( "variant_list.lc",
        ["variant_boolean.lc"; "variant_natural.lc"; "variant_option.lc"] ) ]
  in
  let test_suite =
    "explicitly_typed::isorecursive_types"
    >::: [ Test_term.make ();
           Test_repl.make_explicit repl_exe case_dir repl_cases ]
  in
  run_test_tt_main test_suite
