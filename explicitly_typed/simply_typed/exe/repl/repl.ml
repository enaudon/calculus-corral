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

    let to_kind ?env:_ _ = Kind.Base

  end

  module Term = struct

    include Simply_typed.Term

    let to_type ?env:env_opt tm = match env_opt with
      | None -> to_type tm
      | Some env -> to_type ~env:(snd env) tm

    let to_value ?deep ?env:env_opt tm = match env_opt with
      | None -> beta_reduce ?deep tm
      | Some env -> beta_reduce ?deep ~env:(Misc.fst_of_3 env) tm

  end

  let parse = Simply_typed.Parser.commands Simply_typed.Lexer.prog

  let arg_specs = []

end)

let () = Repl.main ()
