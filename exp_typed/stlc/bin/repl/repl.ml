module Repl = Language.Repl (struct
  module Kind = Stlc.Kind
  module Type = Stlc.Type
  module Term = Stlc.Term
  let parse = Stlc.Parser.commands Stlc.Lexer.prog
end)

let () = Repl.main ()
