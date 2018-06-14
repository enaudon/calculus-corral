module Id = Identifier

type t =
  | Variable of Id.t
  | Function of t * t

(* Internal utilities *)

let base_id = "*"

let var id = Variable id

let func arg res = Function (arg, res)

(* Transformations *)

let rec beta_reduce ?deep ?(env = Id.Map.empty) tp =
  let beta_reduce = beta_reduce ?deep ~env in
  match tp with
    | Variable id ->
      Id.Map.find_default tp id env
    | Function (arg, res) ->
      func (beta_reduce arg) (beta_reduce res)

(* Utilities *) 

let alpha_equivalent ?(beta_env = Id.Map.empty) ?(env=[]) tp1 tp2 =
  let rec alpha_equiv env tp1 tp2 = match tp1, tp2 with
    | Variable id1, Variable id2 ->
      Id.alpha_equivalent env id1 id2
    | Function (arg1, res1), Function (arg2, res2) ->
      alpha_equiv env arg1 arg2 && alpha_equiv env res1 res2
    | _ ->
      false
  in
  alpha_equiv
    env
    (beta_reduce ~deep:() ~env:beta_env tp1)
    (beta_reduce ~deep:() ~env:beta_env tp2)

let rec to_string tp =
  let to_paren_string tp = Printf.sprintf "(%s)" (to_string tp) in
  match tp with
    | Variable id ->
      Id.to_string id
    | Function (arg, res) ->
      let arg_to_string tp = match tp with
        | Variable _ -> to_string tp
        | Function _ -> to_paren_string tp
      in
      Printf.sprintf "%s -> %s" (arg_to_string arg) (to_string res)

(* Constructors *)

let base = var (Id.of_string base_id)

let var id = var @@ Id.of_string id

let func arg res = func arg res

let func' args res =
  List.fold_left (fun res arg -> func arg res) res (List.rev args)

(* Destructors *)

let get_func tp = match tp with
  | Function (arg, res) -> arg, res
  | _ -> invalid_arg "Type.get_func: expected function"
