module Id = Identifier

type t =
  | Proper
  | Operator of t * t

(* Utilities *)

let rec to_intl_repr kn =
  let module IR = Type_operators.Kind in
  match kn with
    | Proper ->
      IR.prop
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
      Id.to_string Id.prop
    | Operator (arg, res) ->
      let arg_to_string kn = match kn with
        | Proper -> to_string kn
        | Operator _ -> to_paren_string kn
      in
      Printf.sprintf "%s %s %s"
        (arg_to_string arg)
        (Id.to_string Id.oper)
        (to_string res)

(* Constructors *)

let prop = Proper

let oper arg res = Operator (arg, res)

let oper' args res = List.fold_right oper args res

(* Destructors *)

let get_oper kn = match kn with
  | Operator (arg, res) -> arg, res
  | _ -> invalid_arg "Kind.get_oper: expected operator"

(* Environment *)

let initial_env =
  [ (Id.func, oper' [prop; prop] prop) ]
