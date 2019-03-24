module Id = Identifier
module Misc = Miscellaneous

module Repl = Language.Repl (struct

  module Value = Universal_types.Term

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

    include Universal_types.Type

    let to_kind env tp =
      check (Id.Set.of_list @@ Kind.Environment.keys env) tp;
      Kind.Base

  end

  module Term = struct

    include Universal_types.Term

    let to_type (kn_env, tp_env) tm =
      let kn_env' = Id.Set.of_list @@ Kind.Environment.keys kn_env in
      to_type (kn_env', tp_env) tm

    let to_value ?deep env tm =
      simplify @@ beta_reduce ?deep (Misc.fst_of_3 env) tm

  end

  let parse = Universal_types.Parser.commands Universal_types.Lexer.prog

  let arg_specs = []

end)

let () = Repl.main ()
