module Id = Identifier
module Misc = Miscellaneous

module Repl = Language.Repl (struct

  module Value = struct

    include Isorecursive_types.Term

    module Environment = Environment.Make (struct
      type value = t
      let initial = []
    end)

  end

  module Kind = struct

    include Isorecursive_types.Kind

    module Environment = Environment.Make (struct
      type value = t
      let initial =
        Id.Map.bindings @@ Isorecursive_types.Type.default_env
    end)

  end

  module Type = struct

    include Isorecursive_types.Type

    module Environment = Type_environment.Make (struct
      type value = t
      let initial_types = []
      let initial_terms = []
    end)

    let to_kind env tp =
      to_kind (Id.Map.of_list @@ Kind.Environment.bindings env) tp

    let beta_reduce ?deep env tp =
      let env' = Id.Map.of_list @@ Environment.type_bindings env in
      beta_reduce ?deep env' tp

  end

  module Term = struct

    include Isorecursive_types.Term

    let to_type (kn_env, tp_env) tm =
      let kn_env' =
        Id.Map.of_list @@ Kind.Environment.bindings kn_env
      in
      let tp_env' =
        Id.Map.of_list @@ Type.Environment.bindings tp_env
      in
      to_type (kn_env', tp_env') tm

    let to_value ?deep (env, _, _) tm =
      let env' = Id.Map.of_list @@ Value.Environment.bindings env in
      beta_reduce ?deep env' tm

  end 
  let parse =
    Isorecursive_types.Parser.commands Isorecursive_types.Lexer.prog

  let arg_specs = []

end)

let () = Repl.main ()
