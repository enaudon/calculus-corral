module Id = Identifier
module Kind_env = Kind.Environment
module Misc = Miscellaneous

type t =
  | Variable of Id.t
  | Abstraction of Id.t * Kind.t * t
  | Application of t * t
  | Universal of Id.t * Kind.t * t

module Env = Type_environment.Make (struct
  type value = t
  let initial_types = []
  let initial_terms = []
end)

module Environment = Env

(* Internal utilities *)

let error : string -> string -> 'a = fun fn_name msg ->
  failwith @@ Printf.sprintf "%s.%s: %s" __MODULE__ fn_name msg

let var : Id.t -> t = fun id -> Variable id

let abs : Id.t -> Kind.t -> t -> t =
  fun arg kn body -> Abstraction (arg, kn, body)

let app : t -> t -> t = fun fn arg -> Application (fn, arg)

let forall : Id.t -> Kind.t -> t -> t = fun quant kn body ->
  Universal (quant, kn, body)

(* Kinding *)

let rec to_kind env tp = match tp with
  | Variable id ->
    begin try Kind_env.find id env with
      | Id.Unbound id ->
        error "to_kind" @@
          Printf.sprintf "undefined identifier '%s'" (Id.to_string id)
    end
  | Abstraction (arg, arg_kn, body) ->
    let body_kn = to_kind (Kind_env.add arg arg_kn env) body in
    Kind.oper arg_kn body_kn
  | Application (fn, arg) ->
    let fn_kn = to_kind env fn in
    let fml_arg_kn, res_kn =
      try
        Kind.get_oper fn_kn
      with Invalid_argument _ ->
        error "to_kind" @@
          Printf.sprintf
            "expected function kind; found '%s'"
            (Kind.to_string fn_kn)
    in
    let act_arg_kn = to_kind env arg in
    if Kind.alpha_equivalent act_arg_kn fml_arg_kn then
      res_kn
    else
      error "to_kind" @@
        Printf.sprintf
          "expected kind '%s'; found kind '%s'"
            (Kind.to_string fml_arg_kn)
            (Kind.to_string act_arg_kn)
  | Universal (quant, kn, body) ->
    to_kind (Kind_env.add quant kn env) body

(* Transformations *)

(**
  [subst] avoids name capture by renaming binders in [tp] to follow the
  Barendregt convention--i.e. the names of bound variable are chosen
  distinct from those of free variables.
 *)
let rec subst fvs sub tp = match tp with
  | Variable id ->
    Env.Type.find_default tp id sub
  | Abstraction (arg, kn, body) when Id.Set.mem arg fvs ->
    let arg' = Id.gen_upper () in
    let sub' = Env.Type.add arg (var arg') sub in
    abs arg' kn @@ subst (Id.Set.add arg' fvs) sub' body
  | Abstraction (arg, kn, body) ->
    abs arg kn @@
      subst (Id.Set.add arg fvs) (Env.Type.del arg sub) body
  | Application (fn, arg) ->
    app (subst fvs sub fn) (subst fvs sub arg)
  | Universal (quant, kn, body) when Id.Set.mem quant fvs ->
    let quant' = Id.gen_upper () in
    let sub' = Env.Type.add quant (var quant') sub in
    forall quant' kn @@ subst (Id.Set.add quant' fvs) sub' body
  | Universal (quant, kn, body) ->
    forall quant kn @@
      subst (Id.Set.add quant fvs) (Env.Type.del quant sub) body

let rec beta_reduce ?deep env tp =

  let beta_reduce = beta_reduce ?deep in

  let subst env tp id tp' =
    let fvs = Id.Set.of_list @@ Env.Type.keys env in
    subst fvs (Env.Type.singleton id tp') tp
  in

  match tp with
    | Variable id ->
      Env.Type.find_default tp id env
    | Abstraction (arg, kn, body) ->
      if deep <> None then
        abs arg kn @@ beta_reduce (Env.Type.del arg env) body
      else
        tp
    | Application (fn, act_arg) ->
      let fn' = beta_reduce env fn in
      let act_arg' = beta_reduce env act_arg in
      begin match fn' with
        | Abstraction (fml_arg, _, body) ->
          let env' = Env.Type.del fml_arg env in
          beta_reduce env' @@ subst env' body fml_arg act_arg'
        | _ ->
          app fn' act_arg'
      end
    | Universal (quant, kn, body) ->
      if deep <> None then
        forall quant kn @@ beta_reduce (Env.Type.del quant env) body
      else
        tp

(* Utilities *)

let alpha_equivalent ?(beta_env = Env.initial) ?(env = []) tp1 tp2 =
  let rec alpha_equiv env tp1 tp2 = match tp1, tp2 with
    | Variable id1, Variable id2 ->
      Id.alpha_equivalent env id1 id2
    | Abstraction (arg1, kn1, body1), Abstraction (arg2, kn2, body2) ->
      Kind.alpha_equivalent kn1 kn2 &&
        alpha_equiv ((arg1, arg2) :: env) body1 body2
    | Application (fn1, arg1), Application (fn2, arg2) ->
      alpha_equiv env fn1 fn2 && alpha_equiv env arg1 arg2
    | Universal (quant1, kn1, body1), Universal (quant2, kn2, body2) ->
      Kind.alpha_equivalent kn1 kn2 &&
        alpha_equiv ((quant1, quant2) :: env) body1 body2
    | _ ->
      false
  in
  alpha_equiv
    env
    (beta_reduce ~deep:() beta_env tp1)
    (beta_reduce ~deep:() beta_env tp2)

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
    | Abstraction (arg, kn, body) when Id.is_generated arg ->
      let arg' = fresh () in
      abs arg' kn @@ simplify (Id.Map.add arg (var arg') env) body
    | Abstraction (arg, kn, body) ->
      abs arg kn @@ simplify env body
    | Application (fn, arg) ->
      let fn' = simplify env fn in
      let arg' = simplify env arg in
      app fn' arg'
    | Universal (quant, kn, body) when Id.is_generated quant ->
      let quant' = fresh () in
      forall quant' kn @@
        simplify (Id.Map.add quant (var quant') env) body
    | Universal (quant, kn, body) ->
      forall quant kn @@ simplify env body
  in

  simplify env tp

let rec to_string tp =
  let to_paren_string tp = Printf.sprintf "(%s)" (to_string tp) in
  let arg_to_string tp = match tp with
    | Variable _ -> to_string tp
    | Abstraction _ | Application _ | Universal _ -> to_paren_string tp
  in
  match tp with
    | Variable id ->
      Id.to_string id
    | Abstraction (arg, kn, body) ->
      Printf.sprintf "\\%s :: %s . %s"
        (Id.to_string arg)
        (Kind.to_string kn)
        (to_string body)
    | Application (Application (Variable id, arg), res)
        when id = Id.func ->
      Printf.sprintf "%s %s %s"
        (arg_to_string arg)
        (Id.to_string id)
        (to_string res)
    | Application (fn, arg) ->
      let fn_to_string tp = match tp with
        | Variable _ | Application _ -> to_string tp
        | Abstraction _ | Universal _ -> to_paren_string tp
      in
      Printf.sprintf "%s %s" (fn_to_string fn) (arg_to_string arg)
    | Universal (quant, kn, body) ->
      Printf.sprintf "forall %s :: %s . %s"
        (Id.to_string quant)
        (Kind.to_string kn)
        (to_string body)

(* Constructors *)

let abs' args body =
  List.fold_right (fun (arg, kn) body -> abs arg kn body) args body

let app' fn args = List.fold_left app fn args

let func arg res = app' (var Id.func) [arg; res]

let func' args res = List.fold_right func args res

let forall' quants body =
  let forall (quant, kn) body = forall quant kn body in
  List.fold_right forall quants body

(* Destructors *)

let get_func tp = match tp with
  | Application (Application (Variable id, arg), res)
      when id = Id.func ->
    arg, res
  | _ -> invalid_arg "Type.get_func: expected function"

let get_forall tp = match tp with
  | Universal (quant, kn, body) -> quant, kn, body
  | _ -> invalid_arg "Type.get_forall: expected universal"

let get_forall' tp =
  let rec get_forall acc tp = match tp with
    | Universal (quant, kn, body) ->
      get_forall ((quant, kn) :: acc) body
    | _ ->
      acc, tp
  in
  let quants, tp = get_forall [] tp in
  List.rev quants, tp
