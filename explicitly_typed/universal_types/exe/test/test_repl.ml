open OUnit

module Misc = Miscellaneous

let assert_command repl dir file prelude () =
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
  let args = "--deep-beta-reduction" :: files in
  assert_command ~exit_code ~foutput repl args

let make repl_exe case_dir =
  "Repl" >:::
    List.map
      ( fun (file, pre) ->
        file >:: assert_command repl_exe case_dir file pre)
      [ ("function.lc", []);
        ("pair.lc", []);
        ("boolean.lc", ["pair.lc"]);
        ("natural.lc", ["pair.lc"]); ]
