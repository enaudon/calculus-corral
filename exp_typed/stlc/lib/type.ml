type t =
  | Base of string
  | Function of t * t

(* Utilities *) 

let rec equals tp1 tp2 = match tp1, tp2 with
  | Base id1, Base id2 ->
    id1 = id2
  | Function (arg1, res1), Function (arg2, res2) ->
    equals arg1 arg2 && equals res1 res2
  | _ ->
    false

let rec to_string tp =
  let to_paren_string tp = Printf.sprintf "(%s)" (to_string tp) in
  match tp with
    | Base id ->
      id
    | Function (arg, res) ->
      let arg_to_string tp = match tp with
        | Base _ -> to_string tp
        | Function _ -> to_paren_string tp
      in
      Printf.sprintf "%s -> %s" (arg_to_string arg) (to_string res)

(* Constructors *)

let base id = Base id

let func arg res = Function (arg, res)

let func' args res =
  List.fold_left (fun res arg -> func arg res) res (List.rev args)

(* Destructors *)

let get_func tp = match tp with
  | Function (arg, res) -> arg, res
  | _ -> invalid_arg "Type.get_func: expected function"
