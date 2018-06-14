module Repl = Language.Repl (struct

  module Value = Universal_types.Term

  module Kind = Universal_types.Kind

  module Type = Universal_types.Type

  module Term = struct 

    include Universal_types.Term

    let to_value = beta_reduce

  end

  let parse = Universal_types.Parser.commands Universal_types.Lexer.prog

  let arg_specs = []

end)

let () = Repl.main ()
