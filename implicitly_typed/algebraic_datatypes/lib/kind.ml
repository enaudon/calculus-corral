type t =
  | Proper
  | Row
  | Operator of t * t

(* Utilities *)

let rec to_intl_repr kn =
  let module IR = Records_and_variants.Kind in
  match kn with
    | Proper ->
      IR.prop
    | Row ->
      IR.row
    | Operator (arg, res) ->
      IR.oper (to_intl_repr arg) (to_intl_repr res)

(**
  There are no kind variables, so alpha-equivalence is just structural
  equivalence.
 *)
let alpha_equivalent = Pervasives.(=)

let rec to_string kn =
  let to_paren_string kn = Printf.sprintf "(%s)" (to_string kn) in
  match kn with
    | Proper ->
      "*"
    | Row ->
      "<>"
    | Operator (arg, res) ->
      let arg_to_string kn = match kn with
        | Proper | Row -> to_string kn
        | Operator _ -> to_paren_string kn
      in
      Printf.sprintf "%s => %s" (arg_to_string arg) (to_string res)

(* Constructors *)

let prop = Proper

let row = Row

let oper arg res = Operator (arg, res)

let oper' args res = List.fold_right oper args res

(* Destructors *)

let get_oper kn = match kn with
  | Operator (arg, res) -> arg, res
  | _ -> invalid_arg "Kind.get_oper: expected operator"
