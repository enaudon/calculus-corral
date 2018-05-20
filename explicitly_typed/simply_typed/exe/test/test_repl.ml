open OUnit

module Misc = Miscellaneous

let assert_command repl file () =
  let exit_code = Unix.WEXITED 0 in
  let foutput stream =
    let actual = Misc.char_stream_to_string stream in
    let expect = Misc.file_to_string (file ^ ".exp") in
    assert_equal expect actual
  in
  assert_command ~exit_code ~foutput repl [file]

let make repl_exe case_dir =
  let test_files =
    let is_lc_file f =
      Sys.file_exists f && not (Sys.is_directory f) &&
        Filename.extension f = ".lc"
    in
    let fs = Array.to_list (Sys.readdir case_dir) in
    List.filter is_lc_file @@ List.map (Filename.concat case_dir) fs
  in
  "Repl" >:::
    List.map (fun f -> f >:: assert_command repl_exe f) test_files
