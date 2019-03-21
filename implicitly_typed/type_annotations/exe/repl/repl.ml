module Id = Identifier
module Misc = Miscellaneous

type type_inference_algorithm =
  | Hindley_milner
  | Pottier_remy

let type_inference_algorithm = ref Pottier_remy

module Repl = Language.Repl (struct

  module Value = struct

    include Type_operators.Term

    module Environment = Environment.Make (struct
      type value = t
      let initial = []
    end)

  end

  module Kind = struct

    include Type_annotations.Kind

    module Environment = Environment.Make (struct
      type value = t
      let initial =
       Id.Map.bindings @@ Type_annotations.Type.Inferencer.default_env
    end)

  end

  module Type = struct

    include Type_annotations.Type

    module Environment = Type_environment.Make (struct
      type value = t
      let initial_types = []
      let initial_terms = []
    end)

    let to_kind env =
      let open Inferencer in
      let register id kn state = register state (var id) kn in
      let state = Kind.Environment.fold register env initial in
      to_kind state

    let beta_reduce ?deep:_ _ _ = assert false

    let to_string tp = to_string tp

  end

  module Term = struct

    include Type_annotations.Term

    let to_type (_, env) tm =
      let to_type = match !type_inference_algorithm with
        | Hindley_milner -> to_type_hm
        | Pottier_remy -> to_type_pr
      in
      to_type (Id.Map.of_list @@ Type.Environment.bindings env) tm

    let to_value ?deep (vl_env, kn_env, tp_env) tm =

      let to_intl_repr = match !type_inference_algorithm with
        | Hindley_milner -> to_intl_repr_hm
        | Pottier_remy -> to_intl_repr_pr
      in

      let tp_env' =
        Id.Map.of_list @@ Type.Environment.bindings tp_env
      in
      let vl = to_intl_repr tp_env' tm in

      let vl_env' =
        Id.Map.of_list @@ Value.Environment.bindings vl_env
      in
      let kn_env' =
        Kind.Environment.bindings kn_env |>
          Id.Map.of_list |>
          Id.Map.map Kind.to_intl_repr
      in
      let tp_env' = Id.Map.map Type.to_intl_repr tp_env' in
      ignore @@ Value.to_type (kn_env', tp_env') vl;
      Value.simplify @@ Value.beta_reduce ?deep vl_env' vl

  end

  let parse =
    Type_annotations.Parser.commands Type_annotations.Lexer.prog

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
