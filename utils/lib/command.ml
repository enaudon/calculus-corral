module Id = Identifier

type ('tp, 'tm) t =
  | Bind_term of Identifier.t * 'tm
  | Eval_term of 'tm

let bind_term id tm = Bind_term (Id.of_string id, tm)

let eval_term tm = Eval_term tm
