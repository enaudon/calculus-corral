(** The type of top-level REPL commands. *)
type ('tp, 'tm) t = private
  | Bind_term of Identifier.t * 'tm
  | Eval_term of 'tm

val bind_term : string -> 'tm -> ('tp, 'tm) t

val eval_term : 'tm -> ('tp, 'tm) t
