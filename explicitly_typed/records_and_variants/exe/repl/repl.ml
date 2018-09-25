module Id = Identifier
module Misc = Miscellaneous

module Repl = Language.Repl (struct

  module Value = Records_and_variants.Term

  module Kind = struct

    type t =
      | Base

    let to_string _ = "*"

  end

  module Type = struct

    include Records_and_variants.Type

    let default_env = Identifier.Map.empty

    let to_kind env tp =
      check (Id.Set.of_list @@ Id.Map.keys env) tp;
      Kind.Base

  end

  module Term = struct

    include Records_and_variants.Term

    let to_type (kn_env, tp_env) tm =
      to_type (Id.Set.of_list @@ Id.Map.keys kn_env, tp_env) tm

    let to_value ?deep env tm =
      simplify @@ beta_reduce ?deep (Misc.fst_of_3 env) tm

  end

  let parse =
    Records_and_variants.Parser.commands Records_and_variants.Lexer.prog

  let arg_specs = []

end)

let () = Repl.main ()
