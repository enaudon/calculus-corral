module Repl = Language.Repl (struct

  module Value = Simply_typed.Term

  module Kind = Simply_typed.Kind

  module Type = Simply_typed.Type

  module Term = struct 

    include Simply_typed.Term

    let to_value = beta_reduce

  end

  let parse = Simply_typed.Parser.commands Simply_typed.Lexer.prog

  let arg_specs = []

end)

let () = Repl.main ()
