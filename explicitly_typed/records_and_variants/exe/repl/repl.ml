module Id = Identifier
module Misc = Miscellaneous

module Repl = Language.Repl (struct

  module Value = struct

    include Records_and_variants.Term

    module Environment = struct
      type env = t Id.Map.t
      let initial = Id.Map.empty
      let add = Id.Map.add
    end

  end

  module Kind = struct

    include Records_and_variants.Kind

    module Environment = struct
      type env = t Id.Map.t
      let initial = Records_and_variants.Type.default_env
      let add = Id.Map.add
    end

  end

  module Type = struct

    include Records_and_variants.Type

    module Environment = struct
      type env = t Id.Map.t
      let initial = Id.Map.empty
      let add_type = Id.Map.add
      let add_term = Id.Map.add
    end

  end

  module Term = struct

    include Records_and_variants.Term

    let to_value ?deep env tm =
      simplify @@ beta_reduce ?deep (Misc.fst_of_3 env) tm

  end

  let parse =
    Records_and_variants.Parser.commands Records_and_variants.Lexer.prog

  let arg_specs = []

end)

let () = Repl.main ()
