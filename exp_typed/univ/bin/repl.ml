module Repl = Language.Repl (struct
  module Kind = Univ.Kind
  module Type = Univ.Type
  module Term = Univ.Term
  let parse = Univ.Parser.commands Univ.Lexer.prog
end)

let () = Repl.main ()
