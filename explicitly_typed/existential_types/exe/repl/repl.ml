module Id = Identifier
module Misc = Miscellaneous

module Repl = Language.Repl (struct

  module Value = struct
          
    include Existential_types.Term

    module Environment = struct
      type env = t Id.Map.t
      let initial = Id.Map.empty
      let add = Id.Map.add
    end

  end

  module Kind = struct

    type t =
      | Base

    module Environment = struct
      type env = t Id.Map.t
      let initial = Id.Map.empty
      let add = Id.Map.add
    end

    let to_string _ = "*"

  end

  module Type = struct

    include Existential_types.Type

    module Environment = struct
      type env = t Id.Map.t
      let initial = Id.Map.empty
      let add_type = Id.Map.add
      let add_term = Id.Map.add
    end

    let to_kind env tp =
      check (Id.Set.of_list @@ Id.Map.keys env) tp;
      Kind.Base

  end

  module Term = struct

    include Existential_types.Term

    let to_type (kn_env, tp_env) tm =
      to_type (Id.Set.of_list @@ Id.Map.keys kn_env, tp_env) tm

    let to_value ?deep env tm = beta_reduce ?deep (Misc.fst_of_3 env) tm

  end

  let parse =
    Existential_types.Parser.commands Existential_types.Lexer.prog

  let arg_specs = []

end)

let () = Repl.main ()
