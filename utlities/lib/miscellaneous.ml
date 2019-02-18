let default_buf_len = 4096

let input_channel_to_string ?(buf_len = default_buf_len) ic =
  let buf = Buffer.create buf_len in
  let rec loop () =
    try
      Buffer.add_channel buf ic buf_len;
      loop ()
    with End_of_file ->
      ()
  in
  loop ();
  Buffer.contents buf

let file_to_string ?(buf_len = default_buf_len) file =
  let ic = open_in file in
  let str =
    try
      input_channel_to_string ~buf_len ic
    with exn ->
      close_in ic;
      raise exn;
  in
  close_in ic;
  str

let char_stream_to_string ?(buf_len = 4096) stream =
  let buf = Buffer.create buf_len in
  Stream.iter (Buffer.add_char buf) stream;
  Buffer.contents buf

let fst_of_3 (x, _, _) = x

let snd_of_3 (_, x, _) = x

let thd_of_3 (_, _, x) = x

let int_to_string a z i =
  let n = z - a + 1 in
  if i / n = 0 then
    Printf.sprintf "%c" (Char.chr @@ a + i mod n)
  else
    Printf.sprintf "%c%n"  (Char.chr @@ a + i mod n) (i / n)

let int_to_lower = int_to_string (Char.code 'a') (Char.code 'z')

let int_to_upper = int_to_string (Char.code 'A') (Char.code 'Z')
