type t =
  | Base
  | Function of t * t

(* Utilities *) 

(**
  [struct_equivalent kn1 kn2] determines whether [kn1] and [kn2] are
  structurally equivalent.  [Pervasives.(=)] implements structural
  equivalence over abitrary types, so [struct_equivalent] just calls
  (=).
 *)
let struct_equivalent = Pervasives.(=)

(**
  [alpha_equivalent kn1 kn2] determines whether [kn1] and [kn2] are
  equivalent up to renaming of variables.  There are no variables in
  kinds, so [alpha_equivalent] just calls [struct_equivalent].
 *)
let alpha_equivalent = struct_equivalent

let rec to_string kn =
  let to_paren_string kn = Printf.sprintf "(%s)" (to_string kn) in
  match kn with
    | Base ->
      "*"
    | Function (arg, res) ->
      let arg_to_string kn = match kn with
        | Base -> to_string kn
        | Function _ -> to_paren_string kn
      in
      Printf.sprintf "%s => %s" (arg_to_string arg) (to_string res)

(* Constructors *)

let base = Base

let func arg res = Function (arg, res)

let func' args res =
  List.fold_left (fun res arg -> func arg res) res (List.rev args)

(* Destructors *)

let get_func kn = match kn with
  | Function (arg, res) -> arg, res
  | _ -> invalid_arg "Kind.get_func: expected function"
