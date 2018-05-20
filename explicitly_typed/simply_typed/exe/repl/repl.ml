module Repl = Language.Repl (struct
  module Kind = Simply_typed.Kind
  module Type = Simply_typed.Type
  module Term = Simply_typed.Term
  let parse = Simply_typed.Parser.commands Simply_typed.Lexer.prog
end)

let () = Repl.main ()
