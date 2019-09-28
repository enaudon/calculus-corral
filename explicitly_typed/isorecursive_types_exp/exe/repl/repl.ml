module Misc = Miscellaneous

module Repl = Language.Repl (struct
  module Value = Isorecursive_types_exp.Term
  module Kind = Isorecursive_types_exp.Kind
  module Type = Isorecursive_types_exp.Type

  module Term = struct
    include Isorecursive_types_exp.Term

    let to_value ?deep (vl_env, _, tp_env) tm =
      simplify @@ beta_reduce ?deep (tp_env, vl_env) tm
  end

  let parse =
    Isorecursive_types_exp.Parser.commands Isorecursive_types_exp.Lexer.prog

  let arg_specs = []
end)

let () = Repl.main ()
