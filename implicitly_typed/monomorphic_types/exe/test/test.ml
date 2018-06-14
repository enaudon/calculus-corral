open OUnit

let parse_cmd_args () =

  (* Command-line arguments *)
  let repl_exe = ref "./repl.exe" in
  let case_dir = ref "./cases/" in

  (* Parse arguments *)
  let arg_specs = [
    ("-repl-exe", Arg.Set_string repl_exe, "The REPL executable.") ;
    ("-case-dir", Arg.Set_string case_dir, "Directory of test cases.") ;
  ] in
  let parse_anon_arg arg =
    failwith @@ Printf.sprintf "Unknown argument '%s'" arg
  in
  let usage_msg = Printf.sprintf "Usage: %s [file]" Sys.argv.(0) in
  Arg.parse arg_specs parse_anon_arg usage_msg;

  (* Sanity checks *)
  if not (Sys.file_exists !repl_exe) then
    failwith @@ Printf.sprintf "'%s' does not exist." !repl_exe;
  if Sys.is_directory !repl_exe then
    failwith @@ Printf.sprintf "'%s' is not a file." !repl_exe;
  if not (Sys.file_exists !case_dir) then
    failwith @@ Printf.sprintf "'%s' does not exist." !case_dir;
  if not (Sys.is_directory !case_dir) then
    failwith @@ Printf.sprintf "'%s' is a not directory." !case_dir;

  !repl_exe, !case_dir

let _ =
  let repl_exe, case_dir = parse_cmd_args () in
  let test_suite = "monomorphic_types" >::: [
      Test_repl.make repl_exe case_dir ;
  ] in
  run_test_tt_main test_suite