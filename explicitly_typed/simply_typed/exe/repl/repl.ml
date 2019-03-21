module Id = Identifier
module Misc = Miscellaneous

module Repl = Language.Repl (struct

  module Value = struct

    include Simply_typed.Term

    module Environment = Environment.Make (struct
      type value = t
      let initial = []
    end)

  end

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

    include Simply_typed.Type

    module Environment = Type_environment.Make (struct
      type value = t
      let initial_types = []
      let initial_terms = []
    end)

    let to_kind env tp =
      check (Id.Set.of_list @@ Kind.Environment.keys env) tp;
      Kind.Base

    let beta_reduce ?deep env tp =
      let env' = Id.Map.of_list @@ Environment.type_bindings env in
      beta_reduce ?deep env' tp

  end

  module Term = struct

    include Simply_typed.Term

    let to_type (_, env) tm =
      to_type (Id.Map.of_list @@ Type.Environment.bindings env) tm

    let to_value ?deep (env, _, _) tm =
      let env' = Id.Map.of_list @@ Value.Environment.bindings env in
      beta_reduce ?deep env' tm

  end

  let parse = Simply_typed.Parser.commands Simply_typed.Lexer.prog

  let arg_specs = []

end)

let () = Repl.main ()
