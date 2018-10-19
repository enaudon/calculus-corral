module Id = Identifier
module Misc = Miscellaneous

type type_inference_algorithm =
  | Hindley_milner
  | Pottier_remy

let type_inference_algorithm = ref Pottier_remy

module Repl = Language.Repl (struct

  module Value = Records_and_variants.Term

  module Kind = Algebraic_datatypes.Kind

  module Type = struct

    include Algebraic_datatypes.Type

    let beta_reduce ?deep:_ _ _ = assert false

    let to_string tp = to_string tp

  end

  module Term = struct

    include Algebraic_datatypes.Term

    let to_type env tm =
      let to_type = match !type_inference_algorithm with
        | Hindley_milner -> to_type_hm
        | Pottier_remy -> to_type_pr
      in
      to_type (snd env) tm

    let to_value ?deep (vl_env, kn_env, tp_env) tm =
      let to_intl_repr = match !type_inference_algorithm with
        | Hindley_milner -> to_intl_repr_hm
        | Pottier_remy -> to_intl_repr_pr
      in
      let vl = to_intl_repr tp_env tm in
      let kn_env' = Id.Map.map Kind.to_intl_repr kn_env in
      let tp_env' = Id.Map.map Type.to_intl_repr tp_env in
      begin try
        ignore @@ Value.to_type (kn_env', tp_env') vl;
      with
        | exn ->
          Printf.printf "  = %s ;\n%!"
            (Value.to_string vl);
          raise exn
      end;
      Value.simplify @@ Value.beta_reduce ?deep vl_env vl

  end

  let parse =
    Algebraic_datatypes.Parser.commands Algebraic_datatypes.Lexer.prog

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
