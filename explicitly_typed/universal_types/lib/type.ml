module Id = Identifier
module Misc = Miscellaneous

type t =
  | Variable of Id.t
  | Function of t * t
  | Universal of Id.t * t

module Env = Type_environment.Make (struct
  type value = t

  let initial_types = []

  let initial_terms = []
end)

module Environment = Env

(* Internal utilities *)

let error : string -> string -> 'a =
 fun fn_name msg ->
   failwith @@ Printf.sprintf "%s.%s: %s" __MODULE__ fn_name msg

(* Constructors *)

let var id = Variable id

let func arg res = Function (arg, res)

let func' args res = List.fold_right func args res

let forall quant body = Universal (quant, body)

let forall' quants body = List.fold_right forall quants body

(* Destructors *)

let get_func tp =
  match tp with
    | Function (arg, res) ->
      (arg, res)
    | _ ->
      invalid_arg "Type.get_func: expected function"

let get_forall tp =
  match tp with
    | Universal (quant, body) ->
      (quant, body)
    | _ ->
      invalid_arg "Type.get_forall: expected universal"

let get_forall' tp =
  let rec get_forall acc tp =
    match tp with
      | Universal (quant, body) ->
        get_forall (quant :: acc) body
      | _ ->
        (acc, tp)
  in
  let quants, tp = get_forall [] tp in
  (List.rev quants, tp)

(* Transformations *)

let reduce env tp =
  match tp with Variable id -> Env.Type.find_default tp id env | _ -> tp

(* External utilities *)

let rec check env tp =
  match tp with
    | Variable id ->
      if not @@ Id.Set.mem id env then
        error "check"
        @@ Printf.sprintf "undefined identifier '%s'" (Id.to_string id)
    | Function (arg, res) ->
      check env arg;
      check env res
    | Universal (quant, body) ->
      check (Id.Set.add quant env) body

let rec alpha_equivalent ?(beta_env = Env.initial) ?(env = []) tp1 tp2 =
  let alpha_equiv beta_env env = alpha_equivalent ~beta_env ~env in
  match (reduce beta_env tp1, reduce beta_env tp2) with
    | Variable id1, Variable id2 ->
      Id.alpha_equivalent env id1 id2
    | Function (arg1, res1), Function (arg2, res2) ->
      alpha_equiv beta_env env arg1 arg2 && alpha_equiv beta_env env res1 res2
    | Universal (quant1, body1), Universal (quant2, body2) ->
      let beta_env' = beta_env |> Env.Type.del quant1 |> Env.Type.del quant2 in
      alpha_equiv beta_env' ((quant1, quant2) :: env) body1 body2
    | _ ->
      false

(** [subst] avoids name capture by renaming binders in [tp] to follow the
    Barendregt convention--i.e. the names of bound variable are chosen distinct
    from those of free variables. *)
let rec subst fvs sub tp =
  match tp with
    | Variable id ->
      Env.Type.find_default tp id sub
    | Function (arg, res) ->
      func (subst fvs sub arg) (subst fvs sub res)
    | Universal (quant, body) when Id.Set.mem quant fvs ->
      let quant' = Id.gen_upper () in
      let sub' = Env.Type.add quant (var quant') sub in
      forall quant' @@ subst (Id.Set.add quant' fvs) sub' body
    | Universal (quant, body) ->
      forall quant @@ subst (Id.Set.add quant fvs) (Env.Type.del quant sub) body

let simplify ?ctx:ctx_opt tp =
  let fresh, env =
    match ctx_opt with
      | None ->
        let cntr = ref (-1) in
        let fresh () =
          incr cntr;
          Id.define @@ Misc.int_to_upper !cntr
        in
        (fresh, Id.Map.empty)
      | Some ctx_opt ->
        ctx_opt
  in
  let rec simplify env tp =
    match tp with
      | Variable id when Id.is_generated id ->
        Id.Map.find_default tp id env
      | Variable _ ->
        tp
      | Function (arg, res) ->
        let arg' = simplify env arg in
        let res' = simplify env res in
        func arg' res'
      | Universal (quant, body) when Id.is_generated quant ->
        let quant' = fresh () in
        forall quant' @@ simplify (Id.Map.add quant (var quant') env) body
      | Universal (quant, body) ->
        forall quant @@ simplify env body
  in
  simplify env tp

let rec to_string tp =
  let to_paren_string tp = Printf.sprintf "(%s)" (to_string tp) in
  match tp with
    | Variable id ->
      Id.to_string id
    | Function (arg, res) ->
      let arg_to_string tp =
        match tp with
          | Variable _ ->
            to_string tp
          | Function _ | Universal _ ->
            to_paren_string tp
      in
      Printf.sprintf "%s -> %s" (arg_to_string arg) (to_string res)
    | Universal (quant, body) ->
      Printf.sprintf "forall %s . %s" (Id.to_string quant) (to_string body)
