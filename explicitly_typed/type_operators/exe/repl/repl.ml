module Repl = Language.Repl (struct
  module Kind = Type_operators.Kind
  module Type = Type_operators.Type
  module Term = Type_operators.Term
  let parse = Type_operators.Parser.commands Type_operators.Lexer.prog
end)

let () = Repl.main ()
