open OUnit

module Misc = Miscellaneous

let assert_command repl inf_alg dir file prelude () =
  let files = List.map (Filename.concat dir) (prelude @ [file]) in
  let exit_code = Unix.WEXITED 0 in
  let foutput stream =
    let actual = Misc.char_stream_to_string stream in
    let expect =
      String.concat "" @@
        List.map (fun f -> Misc.file_to_string (f ^ ".exp")) files
    in
    let printer str = str in
    assert_equal ~printer expect actual
  in
  let args =
    "--deep-beta-reduction" ::
    ("--type-inference-algorithm=" ^ inf_alg) ::
    files
  in
  assert_command ~exit_code ~foutput repl args

let make repl_exe case_dir =
  let make_test type_inf_alg (file, prelude) =
    file >:: assert_command repl_exe type_inf_alg case_dir file prelude
  in
  let tests = [
    ("function.lc", []) ;
    ("pair.lc", []) ;
    ("boolean.lc", ["pair.lc"]) ;
    ("natural.lc", ["pair.lc"]) ;
  ] in
  "Repl" >::: [
    "Hindley_milner" >::: List.map (make_test "hm") tests ;
    "Pottier_remy" >::: List.map (make_test "pr") tests ;
  ]
