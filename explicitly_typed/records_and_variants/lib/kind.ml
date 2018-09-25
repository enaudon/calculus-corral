type t =
  | Base
  | Row
  | Function of t * t

(* Utilities *)

(**
  There are no kind variables, so alpha-equivalence is just structural
  equivalence.
 *)
let alpha_equivalent = Pervasives.(=)

let rec to_string kn =
  let to_paren_string kn = Printf.sprintf "(%s)" (to_string kn) in
  match kn with
    | Base ->
      "*"
    | Row ->
      "row"
    | Function (arg, res) ->
      let arg_to_string kn = match kn with
        | Base | Row -> to_string kn
        | Function _ -> to_paren_string kn
      in
      Printf.sprintf "%s => %s" (arg_to_string arg) (to_string res)

(* Constructors *)

let base = Base

let row = Row

let func arg res = Function (arg, res)

let func' args res = List.fold_right func args res

(* Destructors *)

let get_func kn = match kn with
  | Function (arg, res) -> arg, res
  | _ -> invalid_arg "Kind.get_func: expected function"
