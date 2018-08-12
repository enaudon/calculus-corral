module Misc = Miscellaneous

module Repl = Language.Repl (struct

  module Value = Type_operators.Term

  module Kind = Type_operators.Kind

  module Type = struct

    include Type_operators.Type

    let to_kind env tp = to_kind env tp

  end

  module Term = struct 

    include Type_operators.Term

    let to_value ?deep env tm = beta_reduce ?deep (Misc.fst_of_3 env) tm

  end

  let parse = Type_operators.Parser.commands Type_operators.Lexer.prog

  let arg_specs = []

end)

let () = Repl.main ()
