module Misc = Miscellaneous

module Repl = Language.Repl (struct

  module Value = Records_and_variants.Term

  module Kind = Records_and_variants.Kind

  module Type = Records_and_variants.Type

  module Term = struct

    include Records_and_variants.Term

    let to_value ?deep (vl_env, _, tp_env) tm =
      simplify @@ beta_reduce ?deep (tp_env, vl_env) tm

  end

  let parse =
    Records_and_variants.Parser.commands Records_and_variants.Lexer.prog

  let arg_specs = []

end)

let () = Repl.main ()
