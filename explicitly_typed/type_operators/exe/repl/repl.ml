module Misc = Miscellaneous

module Repl = Language.Repl (struct

  module Value = Type_operators.Term

  module Kind = Type_operators.Kind

  module Type = Type_operators.Type

  module Term = struct 

    include Type_operators.Term

    let to_value ?deep (vl_env, _, tp_env) tm =
      simplify @@ beta_reduce ?deep (tp_env, vl_env) tm

  end

  let parse = Type_operators.Parser.commands Type_operators.Lexer.prog

  let arg_specs = []

end)

let () = Repl.main ()
