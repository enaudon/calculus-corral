(** Locations in source files.

  Locations are represented as a starting row/column and ending
  row/column in a particular file, and they are interpreted as including
  all of the characters between their start and end points.
 *)

(** {1 Types} *)

(** The type of source-code locations. *)
type t

(** {1 Functions} *)

(** [dummy] is a dummy location. *)
val dummy : t

(**
  [of_lex_pos start finish] creates a location beginning at [start] and
  ending at [finish].
 *)
val of_lex_pos : Lexing.position -> Lexing.position -> t

(** [to_string loc] computes a string representation of [loc]. *)
val to_string : t -> string
