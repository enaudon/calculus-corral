module Id = Identifier

type type_inference_algorithm =
  | Hindley_milner
  | Pottier_remy

let type_inference_algorithm = ref Pottier_remy

module Repl = Language.Repl (struct
  module Value = Universal_types.Term

  module Kind = struct
    type t = Base

    module Environment = Environment.Make (struct
      type value = t

      let initial = []
    end)

    let to_string _ = "*"
  end

  module Type = struct
    include Polymorphic_types.Type

    let to_kind _ _ = Kind.Base

    let beta_reduce ?deep:_ _ _ = assert false

    let to_string tp = to_string tp
  end

  module Term = struct
    include Polymorphic_types.Term

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
      let module IR_env = Universal_types.Type.Environment in
      let module Type_env = Type.Environment in
      let to_intl_repr =
        match !type_inference_algorithm with
          | Hindley_milner ->
            to_intl_repr_hm
          | Pottier_remy ->
            to_intl_repr_pr
      in
      let vl = to_intl_repr tp_env tm in
      let kn_env' = Id.Set.of_list @@ Kind.Environment.keys kn_env in
      let tp_env' =
        Type_env.fold
          (fun id tp -> IR_env.Type.add id (Type.to_intl_repr tp))
          (fun id tp -> IR_env.Term.add id (Type.to_intl_repr tp))
          tp_env
          IR_env.empty
      in
      ignore @@ Value.to_type (kn_env', tp_env') vl;
      Value.simplify @@ Value.beta_reduce ?deep (tp_env', vl_env) vl
  end

  let parse = Polymorphic_types.Parser.commands Polymorphic_types.Lexer.prog

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
