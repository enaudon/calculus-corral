module Repl = Language.Repl (struct

  module Value = Existential_types.Term

  module Kind = Existential_types.Kind

  module Type = Existential_types.Type

  module Term = struct 

    include Existential_types.Term

    let to_value = beta_reduce

  end

  let parse =
    Existential_types.Parser.commands Existential_types.Lexer.prog

  let arg_specs = []

end)

let () = Repl.main ()
