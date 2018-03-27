module Id = Identifier

type t =
  | Base of string
  | Variable of Id.t
  | Function of t * t
  | Universal of Id.t * t

(* Internal utilities *)

let error : string -> 'a = fun msg -> failwith msg

let base id = Base id

let var id = Variable id

let func arg res = Function (arg, res)

let univ id tp = Universal (id, tp)

(* External utilities *)

(**
  [struct_equivalent tp1 tp2] determines whether [tp1] and [tp2] are
  structurally equivalent.  Since OCaml's [Pervasives.(=)]  implements
  structural equivalence over abitrary types, [struct_equivalent] just
  calls that directly.
 *)
let struct_equivalent = Pervasives.(=)

let rec alpha_equivalent ?(env=Id.Map.empty) tp1 tp2 =
  let alpha_equiv env = alpha_equivalent ~env in
  match tp1, tp2 with
    | Base id1, Base id2 ->
      id1 = id2
    | Variable id1, Variable id2 ->
      let id1' = try Id.Map.find id1 env with
        | Id.Unbound id ->
          error @@
            Printf.sprintf
              "Type.alpha_equivalent: undefined identifier '%s'"
              (Id.to_string id)
      in
      id1' = id2
    | Function (arg1, res1), Function (arg2, res2) ->
      alpha_equiv env arg1 arg2 && alpha_equiv env res1 res2
    | Universal (id1, tp1), Universal (id2, tp2) ->
      alpha_equiv (Id.Map.add id1 id2 env) tp1 tp2
    | _ ->
      false

let free_vars =
  let rec free_vars fvars tp = match tp with
    | Base _ ->
      fvars
    | Variable id ->
      Id.Set.add id fvars
    | Function (arg, res) ->
      free_vars (free_vars fvars arg) res
    | Universal (arg, body) ->
      Id.Set.del arg @@ free_vars fvars body
  in
  free_vars Id.Set.empty

let rec subst fvars sub tp = match tp with
  | Base _ ->
    tp
  | Variable id ->
    Id.Map.find_default tp id sub
  | Function (arg, res) ->
    func (subst fvars sub arg) (subst fvars sub res)
  | Universal (id, tp) ->
    if Id.Set.mem id fvars then
      let id' = Id.fresh () in
      let sub' = Id.Map.add id (var id') sub in
      univ id' @@ subst (Id.Set.add id' fvars) sub' tp
    else
     univ id @@ subst (Id.Set.add id fvars) (Id.Map.del id sub) tp

let rec to_string tp =
  let to_paren_string tp = Printf.sprintf "(%s)" (to_string tp) in
  match tp with
    | Base id ->
      id
    | Variable id ->
      Id.to_string id
    | Function (arg, res) ->
      let arg_to_string tp = match tp with
        | Base _ | Variable _ -> to_string tp
        | Function _ | Universal _ -> to_paren_string tp
      in
      Printf.sprintf "%s -> %s" (arg_to_string arg) (to_string res)
    | Universal (id, tp) ->
      Printf.sprintf "forall %s . %s" (Id.to_string id) (to_string tp)

(* Constructors *)

let base id = base id

let var id = var @@ Id.of_string id

let func arg res = func arg res

let func' args res =
  List.fold_left (fun res arg -> func arg res) res (List.rev args)

let forall id tp = univ (Id.of_string id) tp

let forall' ids tp =
  List.fold_left (fun tp id -> forall id tp) tp (List.rev ids)

(* Destructors *)

let get_func tp = match tp with
  | Function (arg, res) -> arg, res
  | _ -> invalid_arg "Type.get_func: expected function"

let get_forall tp = match tp with
  | Universal (id, tp) -> id, tp
  | _ -> invalid_arg "Type.get_forall: expected universal"
