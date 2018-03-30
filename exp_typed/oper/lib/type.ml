module Id = Identifier

type t =
  | Constant of Id.t
  | Variable of Id.t
  | Abstraction of Id.t * t
  | Application of t * t

(* Internal utilities *)

let error : string -> string -> 'a = fun fn_name msg ->
  failwith @@ Printf.sprintf "%s.%s: %s" __MODULE__ fn_name msg

let cst : Id.t -> t = fun id -> Constant id

let var : Id.t -> t = fun id -> Variable id

let abs : Id.t -> t -> t = fun arg body -> Abstraction (arg, body)

let app : t -> t -> t = fun fn arg -> Application (fn, arg)

(* Transformations *)

let free_vars : t -> Id.Set.t =
  let rec free_vars fvs tp = match tp with
    | Constant _ -> fvs
    | Variable id -> Id.Set.add id fvs
    | Abstraction (arg, body) -> Id.Set.del arg @@ free_vars fvs body
    | Application (fn, arg) -> free_vars (free_vars fvs fn) arg
  in
  free_vars Id.Set.empty

let subst : ?fvs : Id.Set.t -> t -> Id.t -> t -> t =
    fun ?fvs tp id tp' ->
  let rec subst fvs sub tp = match tp with
    | Constant _ -> tp
    | Variable id ->
      Id.Map.find_default tp id sub
    | Abstraction (arg, body) when Id.Set.mem arg fvs ->
      let arg' = Id.fresh () in
      let sub' = Id.Map.add arg (var arg') sub in
      abs arg' @@ subst (Id.Set.add arg' fvs) sub' body
    | Abstraction (arg, body) ->
      abs arg @@ subst (Id.Set.add arg fvs) (Id.Map.del arg sub) body
    | Application (fn, arg) ->
      app (subst fvs sub fn) (subst fvs sub arg)
  in
  let fvs = match fvs with
    | None -> free_vars tp'
    | Some fvs -> fvs
  in
  subst fvs (Id.Map.singleton id tp') tp

let beta_reduce ?deep =
  let deep = if deep = None then false else true in
  let rec beta_reduce bvs tp = match tp with
    | Constant _ -> tp
    | Variable id ->
      if Id.Set.mem id bvs then
        tp
      else
        error "beta_reduce" @@
          Printf.sprintf "undefined identifier '%s'" (Id.to_string id)
    | Abstraction (arg, body) ->
      if deep then
        abs arg @@ beta_reduce (Id.Set.add arg bvs) body
      else
        tp
    | Application (fn, act_arg) ->
      let fn' = beta_reduce bvs fn in
      let act_arg' = beta_reduce bvs act_arg in
      begin match fn' with
        | Abstraction (fml_arg, body) ->
          let body' = subst ~fvs:bvs body fml_arg act_arg' in
          beta_reduce bvs body'
        | _ ->
          app fn' act_arg'
      end
  in
  beta_reduce Id.Set.empty

(* Utilities *) 

(**
  [struct_equivalent tp1 tp2] determines whether [tp1] and [tp2] are
  structurally equivalent.  [Pervasives.(=)] implements structural
  equivalence over abitrary types, so [struct_equivalent] just calls
  (=).
 *)
let struct_equivalent = Pervasives.(=)

let alpha_equivalent tp1 tp2 =
  let rec alpha_equiv env tp1 tp2 = match tp1, tp2 with
    | Constant id1, Constant id2 ->
      id1 = id2
    | Variable id1, Variable id2 ->
      let id1' = try Id.Map.find id1 env with
        | Id.Unbound id ->
          error "alpha_equivalent" @@
            Printf.sprintf "undefined identifier '%s'" (Id.to_string id)
      in
      id1' = id2
    | Abstraction (arg1, body1), Abstraction (arg2, body2) ->
      alpha_equiv (Id.Map.add arg1 arg2 env) body1 body2
    | Application (fn1, arg1), Application (fn2, arg2) ->
      alpha_equiv env fn1 fn2 && alpha_equiv env arg1 arg2
    | _ ->
      false
  in
  alpha_equiv
    Id.Map.empty
    (beta_reduce ~deep:() tp1)
    (beta_reduce ~deep:() tp2)

let rec to_string tp =
  let to_paren_string tp = Printf.sprintf "(%s)" (to_string tp) in
  let arg_to_string tp = match tp with
    | Constant _ | Variable _ -> to_string tp
    | Abstraction _ | Application _ -> to_paren_string tp
  in
  match tp with
    | Constant id | Variable id ->
      Id.to_string id
    | Abstraction (arg, body) ->
      Printf.sprintf "%s . %s"
        (Id.to_string arg)
        (to_string body)
    | Application (Application (Constant id, arg), res)
        when Id.to_string id = "->" ->
      Printf.sprintf "%s -> %s" (arg_to_string arg) (to_string res)
    | Application (fn, arg) ->
      let fn_to_string tp = match tp with
        | Constant _ | Variable _ | Application _ -> to_string tp
        | Abstraction _ -> to_paren_string tp
      in
      Printf.sprintf "%s %s" (fn_to_string fn) (arg_to_string arg)

(* Constructors *)

let cst id = cst (Id.of_string id)

let var id = var (Id.of_string id)

let abs arg body = abs (Id.of_string arg) body

let abs' args body =
  List.fold_left (fun body arg -> abs arg body) body (List.rev args)

let app = app

let app' fn args =
  List.fold_left (fun fn args -> app fn args) fn args

let func arg res = app' (cst "->") [arg; res]

let func' args res =
  List.fold_left (fun res arg -> func arg res) res (List.rev args)

(* Destructors *)

let get_func tp = match tp with
  | Application (Application (Constant id, arg), res)
      when Id.to_string id = "->" ->
    arg, res
  | _ -> invalid_arg "Type.get_func: expected function"
