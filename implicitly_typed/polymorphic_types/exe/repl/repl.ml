module Id = Identifier
module Misc = Miscellaneous

type type_inference_algorithm =
  | Hindley_milner
  | Pottier_remy

let type_inference_algorithm = ref Pottier_remy

module Repl = Language.Repl (struct

  module Value = Universal_types.Term

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
      match env_opt with
        | None -> to_type tm
        | Some env -> to_type ~env:(snd env) tm

    let to_value ?deep ?env:env_opt tm =
      let to_intl_repr = match !type_inference_algorithm with
        | Hindley_milner -> to_intl_repr_hm
        (* TODO: Implement [to_intl_repr_pr]. *)
        | Pottier_remy -> to_intl_repr_hm
      in
      match env_opt with
        | None ->
          let vl = to_intl_repr tm in
          ignore @@ Value.to_type vl;
          Value.beta_reduce ?deep vl
        | Some env ->
          let tp_env = Misc.thd_of_3 env in
          let vl = to_intl_repr ~env:tp_env tm in
          ignore @@
            Value.to_type ~env:(Id.Map.map Type.to_intl_repr tp_env) vl;
          Value.beta_reduce ?deep ~env:(Misc.fst_of_3 env) vl

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
