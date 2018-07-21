module Misc = Miscellaneous

module Repl = Language.Repl (struct

  module Value = Existential_types.Term

  module Kind = struct

    type t =
      | Base

    let to_string _ = "*"

  end

  module Type = struct

    include Existential_types.Type

    let default_env = Identifier.Map.empty

    let to_kind ?env:_ _ = Kind.Base

  end

  module Term = struct

    include Existential_types.Term

    let to_type ?env:env_opt tm = match env_opt with
      | None -> to_type tm
      | Some env -> to_type ~env:(snd env) tm

    let to_value ?deep ?env:env_opt tm = match env_opt with
      | None ->
        simplify @@ beta_reduce ?deep tm
      | Some env ->
        simplify @@ beta_reduce ?deep ~env:(Misc.fst_of_3 env) tm

  end

  let parse =
    Existential_types.Parser.commands Existential_types.Lexer.prog

  let arg_specs = []

end)

let () = Repl.main ()
