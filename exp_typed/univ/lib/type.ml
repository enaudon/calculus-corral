module Id = Identifier

type t =
  | Variable of Id.t
  | Function of t * t
  | Universal of Id.t * t

(* Internal utilities *)

let var id = Variable id

let func arg res = Function (arg, res)

let univ id tp = Universal (id, tp)

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

(* External utilities *)

let rec alpha_equivalent ?(env=[]) tp1 tp2 =
  let alpha_equiv env = alpha_equivalent ~env in
  match tp1, tp2 with
    | Variable id1, Variable id2 ->
      Id.alpha_equivalent env id1 id2
    | Function (arg1, res1), Function (arg2, res2) ->
      alpha_equiv env arg1 arg2 && alpha_equiv env res1 res2
    | Universal (id1, tp1), Universal (id2, tp2) ->
      alpha_equiv ((id1, id2) :: env) tp1 tp2
    | _ ->
      false

let free_vars =
  let rec free_vars fvs tp = match tp with
    | Variable id -> Id.Set.add id fvs
    | Function (arg, res) -> free_vars (free_vars fvs arg) res
    | Universal (arg, body) -> Id.Set.del arg @@ free_vars fvs body
  in
  free_vars Id.Set.empty

let rec subst fvs sub tp = match tp with
  | Variable id ->
    Id.Map.find_default tp id sub
  | Function (arg, res) ->
    func (subst fvs sub arg) (subst fvs sub res)
  | Universal (id, tp) when Id.Set.mem id fvs ->
    let id' = Id.fresh () in
    let sub' = Id.Map.add id (var id') sub in
    univ id' @@ subst (Id.Set.add id' fvs) sub' tp
  | Universal (id, tp) ->
    univ id @@ subst (Id.Set.add id fvs) (Id.Map.del id sub) tp

let rec to_string tp =
  let to_paren_string tp = Printf.sprintf "(%s)" (to_string tp) in
  match tp with
    | Variable id ->
      Id.to_string id
    | Function (arg, res) ->
      let arg_to_string tp = match tp with
        | Variable _ -> to_string tp
        | Function _ | Universal _ -> to_paren_string tp
      in
      Printf.sprintf "%s -> %s" (arg_to_string arg) (to_string res)
    | Universal (id, tp) ->
      Printf.sprintf "forall %s . %s" (Id.to_string id) (to_string tp)

(* Constructors *)

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
