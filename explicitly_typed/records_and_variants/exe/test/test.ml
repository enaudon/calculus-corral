open OUnit

let _ =

  let repl_exe, case_dir = Test_repl.parse_argv Sys.argv in
  let repl_cases = [
    ("row_polymorphism.lc", []) ;
    ("function.lc", []) ;
    ("church_boolean.lc", []) ;
    ("church_natural.lc", []) ;
    ("church_pair.lc", ["church_boolean.lc"; "church_natural.lc"]) ;
    ("variant_boolean.lc", []) ;
    ("record_pair.lc", ["variant_boolean.lc"]) ;
    ("variant_option.lc", []) ;
  ] in

  let test_suite = "records_and_variants" >::: [
    Test_term.make () ;
    Test_repl.make_explicit repl_exe case_dir repl_cases ;
  ] in

  run_test_tt_main test_suite
