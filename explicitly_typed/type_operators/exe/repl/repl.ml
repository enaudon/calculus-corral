module Id = Identifier
module Misc = Miscellaneous

module Repl = Language.Repl (struct

  module Value = struct

    include Type_operators.Term

    module Environment = struct
      type env = t Id.Map.t
      let initial = Id.Map.empty
      let add = Id.Map.add
    end

  end

  module Kind = struct

    include Type_operators.Kind

    module Environment = struct
      type env = t Id.Map.t
      let initial = Type_operators.Type.default_env
      let add = Id.Map.add
    end

  end

  module Type = struct

    include Type_operators.Type

    module Environment = struct
      type env = t Id.Map.t
      let initial = Id.Map.empty
      let add_type = Id.Map.add
      let add_term = Id.Map.add
    end

  end

  module Term = struct 

    include Type_operators.Term

    let to_value ?deep env tm =
      simplify @@ beta_reduce ?deep (Misc.fst_of_3 env) tm

  end

  let parse = Type_operators.Parser.commands Type_operators.Lexer.prog

  let arg_specs = []

end)

let () = Repl.main ()
