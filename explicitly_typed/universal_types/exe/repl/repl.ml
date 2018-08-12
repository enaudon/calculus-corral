module Misc = Miscellaneous

module Repl = Language.Repl (struct

  module Value = Universal_types.Term

  module Kind = struct

    type t =
      | Base

    let to_string _ = "*"

  end

  module Type = struct

    include Universal_types.Type

    let default_env = Identifier.Map.empty

    let to_kind _ _ = Kind.Base

  end

  module Term = struct

    include Universal_types.Term

    let to_type env tm = to_type (snd env) tm

    let to_value ?deep env tm =
      simplify @@ beta_reduce ?deep (Misc.fst_of_3 env) tm

  end

  let parse = Universal_types.Parser.commands Universal_types.Lexer.prog

  let arg_specs = []

end)

let () = Repl.main ()
