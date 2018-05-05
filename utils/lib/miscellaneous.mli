(** {1 Input} *)

(**
  [file_to_string ~buf_len file] constructs a string containing the
  contents of the [file].  The optional [buf_len] argument is the
  initial size of the internal buffer used to read [file].
 *)
val file_to_string : ?buf_len : int -> string -> string

(**
  [char_string_to_string ~buf_len stream] constructs a string containing
  the contents of the [stream].  The optional [buf_len] argument is the
  initial size of the internal buffer used to read [stream].
 *)
val char_stream_to_string : ?buf_len : int -> char Stream.t -> string
