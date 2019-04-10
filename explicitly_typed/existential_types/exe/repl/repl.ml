module Id = Identifier
module Misc = Miscellaneous

module Repl = Language.Repl (struct

  module Value = Existential_types.Term

  module Kind = struct

    type t =
      | Base

    module Environment = Environment.Make (struct
      type value = t
      let initial = []
    end)

    let to_string _ = "*"

  end

  module Type = struct

    include Existential_types.Type

    let to_kind env tp =
      check (Id.Set.of_list @@ Kind.Environment.keys env) tp;
      Kind.Base

  end

  module Term = struct

    include Existential_types.Term

    let to_type (kn_env, tp_env) tm =
      let kn_env' = Id.Set.of_list @@ Kind.Environment.keys kn_env in
      to_type (kn_env', tp_env) tm

    let to_value ?deep (vl_env, _, tp_env) tm =
      beta_reduce ?deep (tp_env, vl_env) tm

  end

  let parse =
    Existential_types.Parser.commands Existential_types.Lexer.prog

  let arg_specs = []

end)

let () = Repl.main ()
