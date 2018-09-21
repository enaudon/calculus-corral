module Id = Identifier
module Misc = Miscellaneous

type t =
  | Variable of Id.t
  | Function of t * t
  | Universal of Id.t * t
  | Record of (Id.t * t) list
  | Variant of (Id.t * t) list

(* Internal utilities *)

let error : string -> string -> 'a = fun fn_name msg ->
  failwith @@ Printf.sprintf "%s.%s: %s" __MODULE__ fn_name msg

(* Constructors *)

let var id = Variable id

let func arg res = Function (arg, res)

let func' args res = List.fold_right func args res

let forall quant body = Universal (quant, body)

let forall' quants body = List.fold_right forall quants body

let rcrd fields = Record fields

let vrnt cases = Variant cases

(* Destructors *)

let get_func tp = match tp with
  | Function (arg, res) -> arg, res
  | _ -> invalid_arg "Type.get_func: expected function"

let get_forall tp = match tp with
  | Universal (quant, body) -> quant, body
  | _ -> invalid_arg "Type.get_forall: expected universal"

let get_forall' tp =
  let rec get_forall acc tp = match tp with
    | Universal (quant, body) -> get_forall (quant :: acc) body
    | _ -> acc, tp
  in
  get_forall [] tp

let get_rcrd tp = match tp with
  | Record fields -> fields
  | _ -> invalid_arg "Type.get_rcrd: expected record"

let get_vrnt tp = match tp with
  | Variant cases -> cases
  | _ -> invalid_arg "Type.get_vrnt: expected variant"

(* Transformations *)

let rec beta_reduce ?deep env tp =
  let beta_reduce = beta_reduce ?deep in
  match tp with
    | Variable id ->
      begin match Id.Map.find id env with
        | exception Id.Unbound _ -> tp
        | tp -> beta_reduce env tp
      end
    | Function (arg, res) ->
      func (beta_reduce env arg) (beta_reduce env res)
    | Universal (quant, body) ->
      if deep <> None then
        forall quant @@ beta_reduce (Id.Map.del quant env) body
      else
        tp
    | Record fields ->
      rcrd @@ List.map (fun (id, tp) -> id, beta_reduce env tp) fields
    | Variant cases ->
      vrnt @@ List.map (fun (id, tp) -> id, beta_reduce env tp) cases

(* External utilities *)

let rec check env tp = match tp with
  | Variable id ->
    if not @@ Id.Set.mem id env then
      error "check" @@
        Printf.sprintf "undefined identifier '%s'" (Id.to_string id)
  | Function (arg, res) ->
    check env arg;
    check env res
  | Universal (quant, body) ->
    check (Id.Set.add quant env) body
  | Record fields ->
    List.iter (fun (_, tp) -> check env tp) fields
  | Variant cases ->
    List.iter (fun (_, tp) -> check env tp) cases

let alpha_equivalent ?(beta_env = Id.Map.empty) ?(env = []) tp1 tp2 =

  let rec alpha_equiv_adt env =
    List.for_all2 @@
      fun (id1, tp1) (id2, tp2) ->
        Id.alpha_equivalent env id1 id2 && alpha_equiv env tp1 tp2

  and alpha_equiv env tp1 tp2 = match tp1, tp2 with
    | Variable id1, Variable id2 ->
      Id.alpha_equivalent env id1 id2
    | Function (arg1, res1), Function (arg2, res2) ->
      alpha_equiv env arg1 arg2 && alpha_equiv env res1 res2
    | Universal (quant1, body1), Universal (quant2, body2) ->
      alpha_equiv ((quant1, quant2) :: env) body1 body2
    | Record fields1, Record fields2 ->
      alpha_equiv_adt env fields1 fields2
    | Variant cases1, Variant cases2 ->
      alpha_equiv_adt env cases1 cases2
    | _ ->
      false
  in

  let tp1' = beta_reduce ~deep:() beta_env tp1 in
  let tp2' = beta_reduce ~deep:() beta_env tp2 in
  alpha_equiv env tp1' tp2'

let free_vars =
  let rec free_vars fvs tp = match tp with
    | Variable id -> Id.Set.add id fvs
    | Function (arg, res) -> free_vars (free_vars fvs arg) res
    | Universal (quant, body) -> Id.Set.del quant @@ free_vars fvs body
    | Record fields -> List.fold_left free_vars fvs @@ List.map snd fields
    | Variant cases -> List.fold_left free_vars fvs @@ List.map snd cases
  in
  free_vars Id.Set.empty

let rec subst fvs sub tp = match tp with
  | Variable id ->
    Id.Map.find_default tp id sub
  | Function (arg, res) ->
    func (subst fvs sub arg) (subst fvs sub res)
  | Universal (quant, body) when Id.Set.mem quant fvs ->
    let quant' = Id.fresh_upper () in
    let sub' = Id.Map.add quant (var quant') sub in
    forall quant' @@ subst (Id.Set.add quant' fvs) sub' body
  | Universal (quant, body) ->
    forall quant @@
      subst (Id.Set.add quant fvs) (Id.Map.del quant sub) body
  | Record fields ->
    rcrd @@ List.map (fun (id, tp) -> id, subst fvs sub tp) fields
  | Variant cases ->
    vrnt @@ List.map (fun (id, tp) -> id, subst fvs sub tp) cases

let simplify ?ctx:ctx_opt tp =

  let fresh, env = match ctx_opt with
    | None ->
      let cntr = ref (-1) in
      let fresh () =
        incr cntr;
        Id.of_string @@ Misc.int_to_upper !cntr
      in
      fresh, Id.Map.empty
    | Some ctx_opt ->
      ctx_opt
  in

  let rec simplify env tp = match tp with
    | Variable id ->
      Id.Map.find_default tp id env
    | Function (arg, res) ->
      let arg' = simplify env arg in
      let res' = simplify env res in
      func arg' res'
    | Universal (quant, body) ->
      let quant' = fresh () in
      forall quant' @@ simplify (Id.Map.add quant (var quant') env) body
    | Record fields ->
      rcrd @@ List.map (fun (id, tp) -> id, simplify env tp) fields
    | Variant cases ->
      vrnt @@ List.map (fun (id, tp) -> id, simplify env tp) cases
  in

  simplify env tp

let rec to_string tp =

  let to_paren_string tp = Printf.sprintf "(%s)" (to_string tp) in

  let adt_to_string id_tps =
    String.concat "; " @@
      List.map 
        (fun (id, tp) ->
          Printf.sprintf "%s : %s" (Id.to_string id) (to_string tp))
        id_tps
  in

  match tp with
    | Variable id ->
      Id.to_string id
    | Function (arg, res) ->
      let arg_to_string tp = match tp with
        | Variable _ -> to_string tp
        | Function _ | Universal _ -> to_paren_string tp
        | Record _ | Variant _ -> to_string tp
      in
      Printf.sprintf "%s -> %s" (arg_to_string arg) (to_string res)
    | Universal (quant, body) ->
      Printf.sprintf "forall %s . %s"
        (Id.to_string quant)
        (to_string body)
    | Record fields ->
      Printf.sprintf "{%s}" (adt_to_string fields)
    | Variant cases ->
      Printf.sprintf "[%s]" (adt_to_string cases)
