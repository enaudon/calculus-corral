module IR = Algebraic_types_exp

type type_inference_algorithm =
  | Hindley_milner
  | Pottier_remy

let type_inference_algorithm = ref Pottier_remy

module Repl = Language.Repl (struct
  module Value = IR.Term
  module Kind = Algebraic_types_imp.Kind

  module Type = struct
    include Algebraic_types_imp.Type

    let to_string tp = to_string tp
  end

  module Term = struct
    include Algebraic_types_imp.Term

    let to_type env tm =
      let to_type =
        match !type_inference_algorithm with
          | Hindley_milner ->
            to_type_hm
          | Pottier_remy ->
            to_type_pr
      in
      to_type (snd env) tm

    let to_value ?deep (vl_env, kn_env, tp_env) tm =
      let module IR_kind_env = IR.Kind.Environment in
      let module IR_type_env = IR.Type.Environment in
      let module Type_env = Type.Environment in
      let module Kind_env = Kind.Environment in
      let to_intl_repr =
        match !type_inference_algorithm with
          | Hindley_milner ->
            to_intl_repr_hm
          | Pottier_remy ->
            to_intl_repr_pr
      in
      let vl = to_intl_repr tp_env tm in
      let kn_env' =
        Kind_env.fold
          (fun id kn -> IR_kind_env.add id @@ Kind.to_intl_repr kn)
          kn_env
          IR_kind_env.empty
      in
      let tp_env' =
        Type_env.fold
          (fun id tp -> IR_type_env.Type.add id (Type.to_intl_repr tp))
          (fun id tp -> IR_type_env.Term.add id (Type.to_intl_repr tp))
          tp_env
          IR_type_env.empty
      in
      ignore @@ Value.to_type (kn_env', tp_env') vl;
      Value.simplify @@ Value.beta_reduce ?deep (tp_env', vl_env) vl
  end

  let parse = Algebraic_types_imp.Parser.commands Algebraic_types_imp.Lexer.prog

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
