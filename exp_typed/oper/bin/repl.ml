module Repl = Language.Repl (struct
  module Type = Oper.Type
  module Term = Oper.Term
  let parse = Oper.Parser.commands Oper.Lexer.prog
end)

let () = Repl.main ()
