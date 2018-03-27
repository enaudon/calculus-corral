type t =
  | Base
  | Function of t * t

(* Utilities *) 

(**
  [struct_equivalent tp1 tp2] determines whether [tp1] and [tp2] are
  structurally equivalent.  [Pervasives.(=)] implements structural
  equivalence over abitrary types, so [struct_equivalent] just calls
  (=).
 *)
let struct_equivalent = Pervasives.(=)

(**
  [alpha_equivalent tp1 tp2] determines whether [tp1] and [tp2] are
  equivalent up to renaming of variables.  There are no variables in
  types, so [alpha_equivalent] just calls [struct_equivalent].
 *)
let alpha_equivalent = struct_equivalent

let rec to_string tp =
  let to_paren_string tp = Printf.sprintf "(%s)" (to_string tp) in
  match tp with
    | Base ->
      "B"
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
