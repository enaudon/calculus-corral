module Id = Identifier
module Misc = Miscellaneous

type t =
  | Variable of Id.t
  | Function of t * t
  | Existential of Id.t * t

(* Internal utilities *)

let error : string -> string -> 'a = fun fn_name msg ->
  failwith @@ Printf.sprintf "%s.%s: %s" __MODULE__ fn_name msg

(* Constructors *)

let var id = Variable id

let base_id = "*"

let base = var (Id.define base_id)

let func arg res = Function (arg, res)

let func' args res = List.fold_right func args res

let exists quant body = Existential (quant, body)

let exists' quants body = List.fold_right exists quants body

(* Destructors *)

let get_func tp = match tp with
  | Function (arg, res) -> arg, res
  | _ -> invalid_arg "Type.get_func: expected function"

let get_exists tp = match tp with
  | Existential (quant, body) -> quant, body
  | _ -> invalid_arg "Type.get_exists: expected existential"

(* Transformations *)

let rec beta_reduce ?deep env tp =
  let beta_reduce = beta_reduce ?deep env in
  match tp with
    | Variable id ->
      Id.Map.find_default tp id env
    | Function (arg, res) ->
      func (beta_reduce arg) (beta_reduce res)
    | Existential (quant, body) ->
      if deep <> None then
        exists quant @@ beta_reduce body
      else
        tp

(* Utilities *)

let rec check env tp = match tp with
  | Variable id ->
    if not @@ Id.Set.mem id env && id <> Id.define base_id then
      error "check" @@
        Printf.sprintf "undefined identifier '%s'" (Id.to_string id)
  | Function (arg, res) ->
    check env arg;
    check env res
  | Existential (quant, body) ->
    check (Id.Set.add quant env) body

let alpha_equivalent ?(beta_env = Id.Map.empty) ?(env=[]) tp1 tp2 =
  let rec alpha_equiv env tp1 tp2 = match tp1, tp2 with
    | Variable id1, Variable id2 ->
      Id.alpha_equivalent env id1 id2
    | Function (arg1, res1), Function (arg2, res2) ->
      alpha_equiv env arg1 arg2 && alpha_equiv env res1 res2
    | Existential (quant1, body1), Existential (quant2, body2) ->
      alpha_equiv ((quant1, quant2) :: env) body1 body2
    | _ ->
      false
  in
  alpha_equiv
    env
    (beta_reduce ~deep:() beta_env tp1)
    (beta_reduce ~deep:() beta_env tp2)

let free_vars =
  let rec free_vars fvs tp = match tp with
    | Variable id -> Id.Set.add id fvs
    | Function (arg, res) -> free_vars (free_vars fvs arg) res
    | Existential (quant, body) -> Id.Set.del quant @@ free_vars fvs body
  in
  free_vars Id.Set.empty

let rec subst fvs sub tp = match tp with
  | Variable id ->
    Id.Map.find_default tp id sub
  | Function (arg, res) ->
    func (subst fvs sub arg) (subst fvs sub res)
  | Existential (quant, body) when Id.Set.mem quant fvs ->
    let quant' = Id.gen_upper () in
    let sub' = Id.Map.add quant (var quant') sub in
    exists quant' @@ subst (Id.Set.add quant' fvs) sub' body
  | Existential (quant, body) ->
    exists quant @@
      subst (Id.Set.add quant fvs) (Id.Map.del quant sub) body

let simplify ?ctx:ctx_opt tp =

  let fresh, env = match ctx_opt with
    | None ->
      let cntr = ref (-1) in
      let fresh () =
        incr cntr;
        Id.define @@ Misc.int_to_upper !cntr
      in
      fresh, Id.Map.empty
    | Some ctx_opt ->
      ctx_opt
  in

  let rec simplify env tp = match tp with
    | Variable id when Id.is_generated id ->
      Id.Map.find_default tp id env
    | Variable _ ->
      tp
    | Function (arg, res) ->
      let arg' = simplify env arg in
      let res' = simplify env res in
      func arg' res'
    | Existential (quant, body) when Id.is_generated quant ->
      let quant' = fresh () in
      exists quant' @@ simplify (Id.Map.add quant (var quant') env) body
    | Existential (quant, body) ->
      exists quant @@ simplify env body
  in

  simplify env tp

let rec to_string tp =
  let to_paren_string tp = Printf.sprintf "(%s)" (to_string tp) in
  match tp with
    | Variable id ->
      Id.to_string id
    | Function (arg, res) ->
      let arg_to_string tp = match tp with
        | Variable _ -> to_string tp
        | Function _ | Existential _ -> to_paren_string tp
      in
      Printf.sprintf "%s -> %s" (arg_to_string arg) (to_string res)
    | Existential (quant, body) ->
      Printf.sprintf "exists %s . %s"
        (Id.to_string quant)
        (to_string body)
