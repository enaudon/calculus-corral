module Misc = Miscellaneous

module Repl = Language.Repl (struct

  module Value = Isorecursive_types.Term

  module Kind = Isorecursive_types.Kind

  module Type = Isorecursive_types.Type

  module Term = struct

    include Isorecursive_types.Term

    let to_value ?deep (vl_env, _, tp_env) tm =
      simplify @@ beta_reduce ?deep (tp_env, vl_env) tm

  end 

  let parse =
    Isorecursive_types.Parser.commands Isorecursive_types.Lexer.prog

  let arg_specs = []

end)

let () = Repl.main ()
