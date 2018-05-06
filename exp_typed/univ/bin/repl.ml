module Repl = Language.Repl (struct
  module Type = Univ.Type
  module Term = Univ.Term
  let parse = Univ.Parser.commands Univ.Lexer.prog
end)

let () = Repl.main ()
