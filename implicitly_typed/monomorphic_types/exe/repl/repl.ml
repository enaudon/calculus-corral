type type_inference_algorithm =
  | Hindley_milner
  | Pottier_remy

let type_inference_algorithm = ref Pottier_remy

module Repl = Language.Repl (struct
  module Value = struct
    include Monomorphic_types.Term

    module Environment = Environment.Make (struct
      type value = t

      let initial = []
    end)

    let to_string _ = "<value>"
  end

  module Kind = struct
    type t = Base

    module Environment = Environment.Make (struct
      type value = t

      let initial = []
    end)

    let to_string _ = "*"
  end

  module Type = struct
    include Monomorphic_types.Type

    let to_kind _ _ = Kind.Base

    let to_string tp = to_string tp
  end

  module Term = struct
    include Monomorphic_types.Term

    let to_type env tm =
      let to_type =
        match !type_inference_algorithm with
          | Hindley_milner ->
            to_type_hm
          | Pottier_remy ->
            to_type_pr
      in
      to_type (snd env) tm

    let to_value ?deep:_ _ tm = tm
  end

  let parse = Monomorphic_types.Parser.commands Monomorphic_types.Lexer.prog

  let arg_specs =
    [ ( "--type-inference-algorithm",
        Arg.String
          (fun str ->
            match str with
              | "hindley-milner" | "hm" ->
                type_inference_algorithm := Hindley_milner
              | "pottier-remy" | "pr" ->
                type_inference_algorithm := Pottier_remy
              | _ ->
                Printf.eprintf "Unknown type inference algorithm \"%s\"" str;
                exit (-1)),
        "Select the algorithm for type inference." ) ]
end)

let () = Repl.main ()
