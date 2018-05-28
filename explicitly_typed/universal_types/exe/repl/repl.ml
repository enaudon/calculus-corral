module Repl = Language.Repl (struct
  module Kind = Universal_types.Kind
  module Type = Universal_types.Type
  module Term = Universal_types.Term
  let parse = Universal_types.Parser.commands Universal_types.Lexer.prog
  let arg_specs = []
end)

let () = Repl.main ()
