module Repl = Language.Repl (struct
  module Kind = Existential_types.Kind
  module Type = Existential_types.Type
  module Term = Existential_types.Term
  let parse =
    Existential_types.Parser.commands Existential_types.Lexer.prog
  let arg_specs = []
end)

let () = Repl.main ()
