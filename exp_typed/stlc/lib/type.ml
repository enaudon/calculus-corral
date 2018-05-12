module Id = Identifier

type t =
  | Base
  | Function of t * t

(* Kinding *)

let default_env = Id.Map.empty

(** There are no type operators, so all types are of kind [*]. *)
let to_kind ?env _ =
  ignore env;
  Kind.base

(* Transformations *)

(** There are no type operators, so there is nothing to beta-reduce. *)
let beta_reduce ?deep ?env tm =
  ignore deep;
  ignore env;
  tm

(* Utilities *) 

(**
  There are no type variables, so alpha-equivalence is just structural
  equivalence.
 *)
let alpha_equivalent ?beta_env =
  ignore beta_env;
  Pervasives.(=)

let rec to_string tp =
  let to_paren_string tp = Printf.sprintf "(%s)" (to_string tp) in
  match tp with
    | Base ->
      "*"
    | Function (arg, res) ->
      let arg_to_string tp = match tp with
        | Base -> to_string tp
        | Function _ -> to_paren_string tp
      in
      Printf.sprintf "%s -> %s" (arg_to_string arg) (to_string res)
(* Constructors *)

let base = Base

let func arg res = Function (arg, res)

let func' args res =
  List.fold_left (fun res arg -> func arg res) res (List.rev args)

(* Destructors *)

let get_func tp = match tp with
  | Function (arg, res) -> arg, res
  | _ -> invalid_arg "Type.get_func: expected function"
