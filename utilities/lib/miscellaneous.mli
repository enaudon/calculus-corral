(** {1 Input} *)

(** [file_to_string ~buf_len file] constructs a string containing the contents
    of the [file]. The optional [buf_len] argument is the initial size of the
    internal buffer used to read [file]. *)
val file_to_string : ?buf_len:int -> string -> string

(** [char_string_to_string ~buf_len stream] constructs a string containing the
    contents of the [stream]. The optional [buf_len] argument is the initial
    size of the internal buffer used to read [stream]. *)
val char_stream_to_string : ?buf_len:int -> char Stream.t -> string

(** {1 Tuples} *)

(** [fst_of_3 x] projects the first element from a three-element tuple. *)
val fst_of_3 : 'a * 'b * 'c -> 'a

(** [snd_of_3 x] projects the second element from a three-element tuple. *)
val snd_of_3 : 'a * 'b * 'c -> 'b

(** [thd_of_3 x] projects the third element from a three-element tuple. *)
val thd_of_3 : 'a * 'b * 'c -> 'c

(** {1 Conversions} *)

(** [int_to_lower i] coverts [i] to an alphanumeric string representation
    beginning with a lowercase letter. *)
val int_to_lower : int -> string

(** [int_to_upper i] coverts [i] to an alphanumeric string representation
    beginning with a uppercase letter. *)
val int_to_upper : int -> string
