module Repl = Language.Repl (struct

  module Kind = Mono.Kind

  module Type = struct

    include Mono.Type

    let default_env = Identifier.Map.empty

    let to_kind ?env _ =
      ignore env;
      Kind.base

    let beta_reduce ?deep ?env _ =
      ignore deep;
      ignore env;
      assert false

    let to_string tp = to_string tp

  end

  module Term = struct

    include Mono.Term

    let to_type = to_type_pr

    let beta_reduce ?deep ?env tm =
      ignore deep;
      ignore env;
      tm

  end

  let parse = Mono.Parser.commands Mono.Lexer.prog

end)

let () = Repl.main ()
