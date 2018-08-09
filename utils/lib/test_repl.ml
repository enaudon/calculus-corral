open OUnit

module Misc = Miscellaneous

let assert_command foutput repl args files () =
  let exit_code = Unix.WEXITED 0 in
  assert_command ~exit_code ~foutput repl (args @ files)

let assert_command_expect repl args dir file prelude =
  let files = List.map (Filename.concat dir) (prelude @ [file]) in
  let foutput stream =
    let actual = Misc.char_stream_to_string stream in
    let expect =
      String.concat "" @@
        List.map (fun f -> Misc.file_to_string (f ^ ".exp")) files
    in
    let printer str = str in
    assert_equal ~printer expect actual
  in
  assert_command foutput repl args files

let assert_command_success repl args dir file prelude =
  let files = List.map (Filename.concat dir) (prelude @ [file]) in
  assert_command ignore repl args files

module Implicit = struct

  let assert_command_deep repl inf_alg dir file prelude =
    let args = [
      "--deep-beta-reduction" ;
      ("--type-inference-algorithm=" ^ inf_alg) ;
    ] in
    assert_command_expect repl args dir file prelude

  let assert_command_shallow repl inf_alg dir file prelude =
    let args = [ ("--type-inference-algorithm=" ^ inf_alg) ] in
    assert_command_success repl args dir file prelude

  let make repl_exe case_dir cases =

    let make_test deep_redux type_inf_alg (file, prelude) =
      let assert_command =
        if deep_redux then
          assert_command_deep
        else
          assert_command_shallow
      in
      file >:: assert_command repl_exe type_inf_alg case_dir file prelude
    in

    "repl" >::: [
      "deep_redux" >::: [
        "hm_infer" >::: List.map (make_test true "hm") cases ;
        "pr_infer" >::: List.map (make_test true "pr") cases ;
      ] ;
      "shallow_redux" >::: [
        "hm_infer" >::: List.map (make_test false "hm") cases ;
        "pr_infer" >::: List.map (make_test false "pr") cases ;
      ] ;
    ]

end

module Explicit = struct

  let assert_command_deep repl dir file prelude =
    let args = [ "--deep-beta-reduction" ] in
    assert_command_expect repl args dir file prelude


let assert_command_shallow repl dir file prelude =
  assert_command_success repl [] dir file prelude

let make repl_exe case_dir cases =

  let make_test deep_redux (file, pre) =
    let assert_command =
      if deep_redux then assert_command_deep
      else assert_command_shallow
    in
    file >:: assert_command repl_exe case_dir file pre
  in

  "repl" >::: [
    "deep_redux" >::: List.map (make_test true) cases ;
    "shallow_redux" >::: List.map (make_test false) cases ;
  ]

end

let make_implicit = Implicit.make

let make_explicit = Explicit.make
