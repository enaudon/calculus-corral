module Id = Identifier

type t =
  | Variable of Id.t
  | Abstraction of Id.t * Kind.t * t
  | Application of t * t

(* Internal utilities *)

let base_id = "*"

let func_id = "->"

let error : string -> string -> 'a = fun fn_name msg ->
  failwith @@ Printf.sprintf "%s.%s: %s" __MODULE__ fn_name msg

let cst : Id.t -> t = fun id -> Variable id

let var : Id.t -> t = fun id -> Variable id

let abs : Id.t -> Kind.t -> t -> t =
  fun arg kn body -> Abstraction (arg, kn, body)

let app : t -> t -> t = fun fn arg -> Application (fn, arg)

(* Kinding *)

let default_env =
  let open Kind in
  Id.Map.empty |>
    Id.Map.add (Id.of_string base_id) base |>
    Id.Map.add (Id.of_string func_id) (func' [base; base] base)

let to_kind ?(env = default_env) =
  let rec to_kind env tp = match tp with
    | Variable id ->
      begin try Id.Map.find id env with
        | Id.Unbound id ->
          error "to_kind" @@
            Printf.sprintf "undefined identifier '%s'" (Id.to_string id)
      end
    | Abstraction (arg, arg_tp, body) ->
      let body_tp = to_kind (Id.Map.add arg arg_tp env) body in
      Kind.func arg_tp body_tp
    | Application (fn, arg) ->
      let fn' = to_kind env fn in
      let fml_arg_tp, res_tp =
        try
          Kind.get_func fn'
        with Invalid_argument _ ->
          error "to_kind" @@
            Printf.sprintf
              "expected function kind; found '%s'"
              (Kind.to_string fn')
      in
      let act_arg_tp = to_kind env arg in
      if Kind.alpha_equivalent act_arg_tp fml_arg_tp then
        res_tp
      else
          error "to_kind" @@
            Printf.sprintf
              "expected kind '%s'; found kind '%s'"
                (Kind.to_string fml_arg_tp)
                (Kind.to_string act_arg_tp)
  in
  to_kind env

(* Transformations *)

let free_vars : t -> Id.Set.t =
  let rec free_vars fvs tp = match tp with
    | Variable id -> Id.Set.add id fvs
    | Abstraction (arg, _, body) -> Id.Set.del arg @@ free_vars fvs body
    | Application (fn, arg) -> free_vars (free_vars fvs fn) arg
  in
  free_vars Id.Set.empty

(**
  [subst tp id tp'] replaces occurences of [id] in [tp] with [tp'].

  [subst] avoids name capture by renaming binders in [tp] to follow the
  Barendregt convention--i.e. the names of bound variable are chosen
  distinct from those of free variables.
 *)
let subst : t -> Id.t -> t -> t = fun tp id tp' ->
  let rec subst fvs sub tp = match tp with
    | Variable id ->
      Id.Map.find_default tp id sub
    | Abstraction (arg, kn, body) when Id.Set.mem arg fvs ->
      let arg' = Id.fresh () in
      let sub' = Id.Map.add arg (var arg') sub in
      abs arg' kn @@ subst (Id.Set.add arg' fvs) sub' body
    | Abstraction (arg, kn, body) ->
      abs arg kn @@
        subst (Id.Set.add arg fvs) (Id.Map.del arg sub) body
    | Application (fn, arg) ->
      app (subst fvs sub fn) (subst fvs sub arg)
  in
  subst (free_vars tp') (Id.Map.singleton id tp') tp

let rec beta_reduce ?deep ?(env = Id.Map.empty) tp =
  let beta_reduce = beta_reduce ?deep ~env in
  match tp with
    | Variable id ->
      Id.Map.find_default tp id env
    | Abstraction (arg, kn, body) ->
      if deep <> None then
        abs arg kn @@ beta_reduce body
      else
        tp
    | Application (fn, act_arg) ->
      let fn' = beta_reduce fn in
      let act_arg' = beta_reduce act_arg in
      begin match fn' with
        | Abstraction (fml_arg, _, body) ->
          let body' = subst body fml_arg act_arg' in
          beta_reduce body'
        | _ ->
          app fn' act_arg'
      end

(* Utilities *) 

let alpha_equivalent tp1 tp2 =
  let rec alpha_equiv env tp1 tp2 = match tp1, tp2 with
    | Variable id1, Variable id2 ->
      Id.alpha_equivalent env id1 id2
    | Abstraction (arg1, kn1, body1), Abstraction (arg2, kn2, body2) ->
      Kind.alpha_equivalent kn1 kn2 &&
        alpha_equiv ((arg1, arg2) :: env) body1 body2
    | Application (fn1, arg1), Application (fn2, arg2) ->
      alpha_equiv env fn1 fn2 && alpha_equiv env arg1 arg2
    | _ ->
      false
  in
  alpha_equiv [] (beta_reduce ~deep:() tp1) (beta_reduce ~deep:() tp2)

let rec to_string tp =
  let to_paren_string tp = Printf.sprintf "(%s)" (to_string tp) in
  let arg_to_string tp = match tp with
    | Variable _ -> to_string tp
    | Abstraction _ | Application _ -> to_paren_string tp
  in
  match tp with
    | Variable id ->
      Id.to_string id
    | Abstraction (arg, kn, body) ->
      Printf.sprintf "%s :: %s . %s"
        (Id.to_string arg)
        (Kind.to_string kn)
        (to_string body)
    | Application (Application (Variable id, arg), res)
        when Id.to_string id = func_id ->
      Printf.sprintf "%s %s %s"
        (arg_to_string arg)
        func_id
        (to_string res)
    | Application (fn, arg) ->
      let fn_to_string tp = match tp with
        | Variable _ | Application _ -> to_string tp
        | Abstraction _ -> to_paren_string tp
      in
      Printf.sprintf "%s %s" (fn_to_string fn) (arg_to_string arg)

(* Constructors *)

let base = cst (Id.of_string base_id)

let var id = var (Id.of_string id)

let abs arg kn body = abs (Id.of_string arg) kn body

let abs' args body =
  let abs' body (arg, kn) = abs arg kn body in
  List.fold_left abs' body (List.rev args)

let app = app

let app' fn args =
  List.fold_left (fun fn args -> app fn args) fn args

let func arg res = app' (cst (Id.of_string func_id)) [arg; res]

let func' args res =
  List.fold_left (fun res arg -> func arg res) res (List.rev args)

(* Destructors *)

let get_func tp = match tp with
  | Application (Application (Variable id, arg), res)
      when Id.to_string id = func_id ->
    arg, res
  | _ -> invalid_arg "Type.get_func: expected function"
