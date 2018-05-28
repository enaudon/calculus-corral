type type_inference_algorithm =
  | Hindley_milner
  | Pottier_remy

let type_inference_algorithm = ref Pottier_remy

module Repl = Language.Repl (struct

  module Kind = Mono.Kind

  module Type = struct

    include Mono.Type

    let default_env = Identifier.Map.empty

    let to_kind ?env _ =
      ignore env;
      Kind.base

    let beta_reduce ?deep ?env _ =
      ignore deep;
      ignore env;
      assert false

    let to_string tp = to_string tp

  end

  module Term = struct

    include Mono.Term

    let to_type ?env tm = match !type_inference_algorithm with
      | Hindley_milner -> to_type_hm ?env tm
      | Pottier_remy -> to_type_pr ?env tm

  end

  let parse = Mono.Parser.commands Mono.Lexer.prog

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
