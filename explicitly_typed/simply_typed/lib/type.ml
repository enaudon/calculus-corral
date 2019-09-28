module Id = Identifier

type t =
  | Variable of Id.t
  | Function of t * t

module Env = Type_environment.Make (struct
  type value = t

  let initial_types = []

  let initial_terms = []
end)

module Environment = Env

(* Internal utilities *)

let error : string -> string -> 'a =
 fun fn_name msg ->
   failwith @@ Printf.sprintf "%s.%s: %s" __MODULE__ fn_name msg

(* Constructors *)

let var id = Variable id

let base_id = "*"

let base = var (Id.define base_id)

let func arg res = Function (arg, res)

let func' args res = List.fold_right func args res

(* Destructors *)

let get_func tp =
  match tp with
    | Function (arg, res) ->
      (arg, res)
    | _ ->
      invalid_arg "Type.get_func: expected function"

(* Transformations *)

let rec beta_reduce ?deep env tp =
  let beta_reduce = beta_reduce ?deep env in
  match tp with
    | Variable id ->
      Env.Type.find_default tp id env
    | Function (arg, res) ->
      func (beta_reduce arg) (beta_reduce res)

(* External utilities *)

let rec check env tp =
  match tp with
    | Variable id ->
      if (not @@ Id.Set.mem id env) && id <> Id.define base_id then
        error "check"
        @@ Printf.sprintf "undefined identifier '%s'" (Id.to_string id)
    | Function (arg, res) ->
      check env arg;
      check env res

let alpha_equivalent ?(beta_env = Env.initial) ?(env = []) tp1 tp2 =
  let rec alpha_equiv env tp1 tp2 =
    match (tp1, tp2) with
      | Variable id1, Variable id2 ->
        Id.alpha_equivalent env id1 id2
      | Function (arg1, res1), Function (arg2, res2) ->
        alpha_equiv env arg1 arg2 && alpha_equiv env res1 res2
      | _ ->
        false
  in
  alpha_equiv
    env
    (beta_reduce ~deep:() beta_env tp1)
    (beta_reduce ~deep:() beta_env tp2)

let rec to_string tp =
  let to_paren_string tp = Printf.sprintf "(%s)" (to_string tp) in
  match tp with
    | Variable id ->
      Id.to_string id
    | Function (arg, res) ->
      let arg_to_string tp =
        match tp with
          | Variable _ ->
            to_string tp
          | Function _ ->
            to_paren_string tp
      in
      Printf.sprintf "%s -> %s" (arg_to_string arg) (to_string res)
