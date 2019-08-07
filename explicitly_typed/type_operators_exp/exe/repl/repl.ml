module Misc = Miscellaneous

module Repl = Language.Repl (struct

  module Value = Type_operators_exp.Term

  module Kind = Type_operators_exp.Kind

  module Type = Type_operators_exp.Type

  module Term = struct 

    include Type_operators_exp.Term

    let to_value ?deep (vl_env, _, tp_env) tm =
      simplify @@ beta_reduce ?deep (tp_env, vl_env) tm

  end

  let parse =
    Type_operators_exp.Parser.commands Type_operators_exp.Lexer.prog

  let arg_specs = []

end)

let () = Repl.main ()
