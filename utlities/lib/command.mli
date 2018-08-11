(** The type of top-level REPL commands. *)
type ('tp, 'tm) t = private
  | Bind_type of Identifier.t * 'tp
  | Bind_term of Identifier.t * 'tm
  | Eval_term of 'tm

(** [bind_type id tp] constructs a command to bind [id] to [tp]. *)
val bind_type : string -> 'tp -> ('tp, 'tm) t

(** [bind_term id tm] constructs a command to bind [id] to [tm]. *)
val bind_term : string -> 'tm -> ('tp, 'tm) t

(** [eval_term tm] constructs a command to evaluate [tm]. *)
val eval_term : 'tm -> ('tp, 'tm) t
