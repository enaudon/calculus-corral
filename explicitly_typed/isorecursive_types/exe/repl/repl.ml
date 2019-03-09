module Id = Identifier
module Misc = Miscellaneous

module Repl = Language.Repl (struct

  module Value = struct

    include Isorecursive_types.Term

    module Environment = struct
      type env = t Id.Map.t
      let initial = Id.Map.empty
      let add = Id.Map.add
    end

  end

  module Kind = struct

    include Isorecursive_types.Kind

    module Environment = struct
      type env = t Id.Map.t
      let initial = Isorecursive_types.Type.default_env
      let add = Id.Map.add
    end

  end

  module Type = struct

    include Isorecursive_types.Type

    module Environment = struct
      type env = t Id.Map.t
      let initial = Id.Map.empty
      let add_type = Id.Map.add
      let add_term = Id.Map.add
    end

  end

  module Term = struct

    include Isorecursive_types.Term

    let to_value ?deep env tm =
      simplify @@ beta_reduce ?deep (Misc.fst_of_3 env) tm

  end

  let parse =
    Isorecursive_types.Parser.commands Isorecursive_types.Lexer.prog

  let arg_specs = []

end)

let () = Repl.main ()
