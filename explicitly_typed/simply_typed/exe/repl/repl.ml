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

    (* There are no type operators, so all types are of kind [*]. *)
    let to_kind ?env _ =
      ignore env;
      Kind.Base

  end

  module Term = struct

    include Simply_typed.Term

    let to_type ?env:env_opt tm = match env_opt with
      | None -> to_type tm
      | Some env -> to_type ~env:(snd env) tm

    let to_value = beta_reduce

  end

  let parse = Simply_typed.Parser.commands Simply_typed.Lexer.prog

  let arg_specs = []

end)

let () = Repl.main ()
