module Id = Identifier
module Misc = Miscellaneous

module Repl = Language.Repl (struct

  module Value = Simply_typed.Term

  module Kind = struct

    type t =
      | Base

    let to_string _ = "*"

  end

  module Type = struct

    include Simply_typed.Type

    let default_env = Identifier.Map.empty

    let to_kind env tp =
      check (Id.Set.of_list @@ Id.Map.keys env) tp;
      Kind.Base

  end

  module Term = struct

    include Simply_typed.Term

    let to_type env tm = to_type (snd env) tm

    let to_value ?deep env tm = beta_reduce ?deep (Misc.fst_of_3 env) tm

  end

  let parse = Simply_typed.Parser.commands Simply_typed.Lexer.prog

  let arg_specs = []

end)

let () = Repl.main ()
