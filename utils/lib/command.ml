module Id = Identifier

type ('tp, 'tm) t =
  | Bind_type of Id.t * 'tp
  | Bind_term of Id.t * 'tm
  | Eval_term of 'tm

let bind_type id tp = Bind_type (Id.of_string id, tp)

let bind_term id tm = Bind_term (Id.of_string id, tm)

let eval_term tm = Eval_term tm
