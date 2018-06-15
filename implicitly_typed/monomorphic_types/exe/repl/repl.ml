module Misc = Miscellaneous

type type_inference_algorithm =
  | Hindley_milner
  | Pottier_remy

let type_inference_algorithm = ref Pottier_remy

module Repl = Language.Repl (struct

  module Value = Monomorphic_types.Term

  module Kind = struct

    type t =
      | Base

    let to_string _ = "*"

  end

  module Type = struct

    include Monomorphic_types.Type

    let default_env = Identifier.Map.empty

    let to_kind ?env _ =
      ignore env;
      Kind.Base

    let beta_reduce ?deep ?env _ =
      ignore deep;
      ignore env;
      assert false

    let to_string tp = to_string tp

  end

  module Term = struct

    include Monomorphic_types.Term

    let to_type ?env:env_opt tm =
      let to_type = match !type_inference_algorithm with
        | Hindley_milner -> to_type_hm
        | Pottier_remy -> to_type_pr
      in
      match env_opt with
        | None -> to_type tm
        | Some env -> to_type ~env:(snd env) tm


    let to_value ?deep ?env:env_opt tm = match env_opt with
      | None -> beta_reduce ?deep tm
      | Some env -> beta_reduce ?deep ~env:(Misc.fst_of_3 env) tm

  end

  let parse =
    Monomorphic_types.Parser.commands Monomorphic_types.Lexer.prog

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
