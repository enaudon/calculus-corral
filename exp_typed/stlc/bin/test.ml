open OUnit

let _ =
  let test_suite = "simply_typed_lambda_calculus" >::: [
      Test_term.make () ;
  ] in
  run_test_tt_main test_suite
