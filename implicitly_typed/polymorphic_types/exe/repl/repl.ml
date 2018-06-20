module Misc = Miscellaneous

type type_inference_algorithm =
  | Hindley_milner
  | Pottier_remy

let type_inference_algorithm = ref Pottier_remy

module Repl = Language.Repl (struct

  module Value = struct

    include Polymorphic_types.Term

    let to_string _ = "<value>"

  end

  module Kind = struct

    type t =
      | Base

    let to_string _ = "*"

  end

  module Type = struct

    include Polymorphic_types.Type

    let default_env = Identifier.Map.empty

    let to_kind ?env:_ _ = Kind.Base

    let beta_reduce ?deep:_ ?env:_ _ = assert false

    let to_string tp = to_string tp

  end

  module Term = struct

    include Polymorphic_types.Term

    let to_type ?env:env_opt tm =
      let to_type = match !type_inference_algorithm with
        | Hindley_milner -> to_type_hm
        | Pottier_remy -> to_type_pr
      in
      let tp = match env_opt with
        | None -> to_type tm
        | Some env -> to_type ~env:(snd env) tm
      in
      Type.gen 1 tp

    let to_value ?deep:_ ?env:_ tm = tm

  end

  let parse =
    Polymorphic_types.Parser.commands Polymorphic_types.Lexer.prog

  let arg_specs = [
    ( "--type-inference-algorithm",
      Arg.String ( fun str -> match str with
        | "hindley-milner" | "hm" ->
          type_inference_algorithm := Hindley_milner
        | "pottier-remy" | "pr" ->
          type_inference_algorithm := Pottier_remy
        | _ ->
          Printf.eprintf "Unknown type inference algorithm \"%s\"" str;
          exit (-1)
      ),
      "Select the algorithm for type inference." ) ;
  ]

end)

let () = Repl.main ()
