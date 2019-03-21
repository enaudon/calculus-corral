module Id = Identifier
module Misc = Miscellaneous

type t =
  | Variable of Id.t
  | Abstraction of Id.t * Kind.t * t
  | Application of t * t
  | Universal of Id.t * Kind.t * t
  | Recursive of Id.t * Kind.t * t
  | Row_nil
  | Row_cons of Id.t * t * t

(* Internal utilities *)

let error : string -> string -> 'a = fun fn_name msg ->
  failwith @@ Printf.sprintf "%s.%s: %s" __MODULE__ fn_name msg

let var : Id.t -> t = fun id -> Variable id

let abs : Id.t -> Kind.t -> t -> t =
  fun arg kn body -> Abstraction (arg, kn, body)

let app : t -> t -> t = fun fn arg -> Application (fn, arg)

let forall : Id.t -> Kind.t -> t -> t = fun quant kn body ->
  Universal (quant, kn, body)

let mu : Id.t -> Kind.t -> t -> t = fun quant kn body ->
  Recursive (quant, kn, body)

let row_nil : t = Row_nil

let row_cons : Id.t -> t -> t -> t = fun id tp rest ->
  Row_cons (id, tp, rest)

let row_of_list : (Id.t * t) list -> t option -> t =
    fun fields rest_opt ->
  let cons (id, tp) rest = row_cons id tp rest in
  let nil = Option.default row_nil rest_opt in
  List.fold_right cons fields nil

let row_to_list : t -> (Id.t * t) list * t option = fun row ->
  let rec to_list acc tp = match tp with
    | Row_nil -> acc, None
    | Row_cons (id, tp, rest) -> to_list ((id, tp) :: acc) rest
    | _ -> acc, Some tp
  in
  let fields, rest = to_list [] row in
  List.rev fields, rest

(* Kinding *)

let rec to_kind env tp = match tp with
  | Variable id ->
    begin try Id.Map.find id env with
      | Id.Unbound id ->
        error "to_kind" @@
          Printf.sprintf "undefined identifier '%s'" (Id.to_string id)
    end
  | Abstraction (arg, arg_kn, body) ->
    let body_kn = to_kind (Id.Map.add arg arg_kn env) body in
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
    to_kind (Id.Map.add quant kn env) body
  (* TODO: check that this case is correct. *)
  | Recursive (quant, kn, body) ->
    let kn' = to_kind (Id.Map.add quant kn env) body in
    if not @@ Kind.alpha_equivalent kn kn' then
      error "to_kind" @@
        Printf.sprintf
          "expected kind '%s'; found kind '%s'"
            (Kind.to_string kn)
            (Kind.to_string kn')
    else
      kn'
  | Row_nil ->
    Kind.row
  | Row_cons (_, tp, rest) ->
    ignore @@ to_kind env tp;
    let rest_kn = to_kind env rest in
    if Kind.alpha_equivalent rest_kn Kind.row then
      Kind.row
    else
      error "to_kind" @@
        Printf.sprintf
          "expected row kind; found kind '%s'"
            (Kind.to_string rest_kn)

(* Transformations *)

let free_vars : t -> Id.Set.t =
  let rec free_vars fvs tp = match tp with
    | Variable id ->
      Id.Set.add id fvs
    | Abstraction (arg, _, body) ->
      Id.Set.del arg @@ free_vars fvs body
    | Application (fn, arg) ->
      free_vars (free_vars fvs fn) arg
    | Universal (quant, _, body) | Recursive (quant, _, body) ->
      Id.Set.del quant @@ free_vars fvs body
    | Row_nil ->
      fvs
    | Row_cons (_, tp, rest) ->
      free_vars (free_vars fvs tp) rest
  in
  (* TODO: Think, would [Kind.initial_env] be safe here? *)
  free_vars Id.Set.empty

(**
  [subst] avoids name capture by renaming binders in [tp] to follow the
  Barendregt convention--i.e. the names of bound variable are chosen
  distinct from those of free variables.
 *)
let rec subst fvs sub tp = match tp with
  | Variable id ->
    Id.Map.find_default tp id sub
  | Abstraction (arg, kn, body) when Id.Set.mem arg fvs ->
    let arg' = Id.gen_upper () in
    let sub' = Id.Map.add arg (var arg') sub in
    abs arg' kn @@ subst (Id.Set.add arg' fvs) sub' body
  | Abstraction (arg, kn, body) ->
    abs arg kn @@
      subst (Id.Set.add arg fvs) (Id.Map.del arg sub) body
  | Application (fn, arg) ->
    app (subst fvs sub fn) (subst fvs sub arg)
  | Universal (quant, kn, body) when Id.Set.mem quant fvs ->
    let quant' = Id.gen_upper () in
    let sub' = Id.Map.add quant (var quant') sub in
    forall quant' kn @@ subst (Id.Set.add quant' fvs) sub' body
  | Universal (quant, kn, body) ->
    forall quant kn @@
      subst (Id.Set.add quant fvs) (Id.Map.del quant sub) body
  | Recursive (quant, kn, body) when Id.Set.mem quant fvs ->
    let quant' = Id.gen_upper () in
    let sub' = Id.Map.add quant (var quant') sub in
    mu quant' kn @@ subst (Id.Set.add quant' fvs) sub' body
  | Recursive (quant, kn, body) ->
    mu quant kn @@
      subst (Id.Set.add quant fvs) (Id.Map.del quant sub) body
  | Row_nil ->
    row_nil
  | Row_cons (id, tp, rest) ->
    row_cons id (subst fvs sub tp) (subst fvs sub rest)

let rec beta_reduce ?deep env tp =
  let beta_reduce = beta_reduce ?deep in
  match tp with
    | Variable id ->
      Id.Map.find_default tp id env
    | Abstraction (arg, kn, body) ->
      if deep <> None then
        abs arg kn @@ beta_reduce env body
      else
        tp
    | Application (fn, act_arg) ->
      let fn' = beta_reduce env fn in
      let act_arg' = beta_reduce env act_arg in
      begin match fn' with
        | Abstraction (fml_arg, _, body) ->
          let sub = Id.Map.singleton fml_arg act_arg' in
          let body' = subst (free_vars act_arg') sub body in
          beta_reduce env body'
        | _ ->
          app fn' act_arg'
      end
    | Universal (quant, kn, body) ->
      if deep <> None then
        forall quant kn @@ beta_reduce (Id.Map.del quant env) body
      else
        tp
    | Recursive (quant, kn, body) ->
      if deep <> None then
        mu quant kn @@ beta_reduce (Id.Map.del quant env) body
      else
        tp
    | Row_nil ->
      row_nil
    | Row_cons (id, tp, rest) ->
      row_cons id (beta_reduce env tp) (beta_reduce env rest)

(* Utilities *)

let alpha_equivalent ?(beta_env = Id.Map.empty) ?(env = []) tp1 tp2 =

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
    | Recursive (quant1, kn1, body1), Recursive (quant2, kn2, body2) ->
      Kind.alpha_equivalent kn1 kn2 &&
        alpha_equiv ((quant1, quant2) :: env) body1 body2
    | Row_nil, Row_nil | Row_cons _, Row_cons _ ->
      let alpha_equiv_rest env rest1 rest2 = match rest1, rest2 with
        | None, None -> true
        | None, _ | _, None -> false
        | Some r1, Some r2 -> alpha_equiv env r1 r2
      in
      let alpha_equiv_row env row1 row2 =
        let alpha_equiv_field env (id1, tp1) (id2, tp2) =
          Id.alpha_equivalent env id1 id2 && alpha_equiv env tp1 tp2
        in
        let compare_fields (id1, _) (id2, _) =
          String.compare (Id.to_string id1) (Id.to_string id2)
        in
        let fields1, rest1 = row_to_list row1 in
        let fields2, rest2 = row_to_list row2 in
        List.for_all2
          (alpha_equiv_field env)
          (List.sort compare_fields fields1)
          (List.sort compare_fields fields2) &&
          alpha_equiv_rest env rest1 rest2
      in
      alpha_equiv_row env tp1 tp2
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
    | Recursive (quant, kn, body) when Id.is_generated quant ->
      let quant' = fresh () in
      mu quant' kn @@ simplify (Id.Map.add quant (var quant') env) body
    | Recursive (quant, kn, body) ->
      mu quant kn @@ simplify env body
    | Row_nil ->
      row_nil
    | Row_cons (id, tp, rest) ->
      let tp' = simplify env tp in
      let rest' = simplify env rest in
      row_cons id tp' rest'
  in

  simplify env tp

let rec to_string tp =

  let to_paren_string tp = Printf.sprintf "(%s)" (to_string tp) in

  let arg_to_string tp = match tp with
    | Application (Variable id, _) when id = Id.rcrd || id = Id.vrnt ->
      to_string tp
    | Variable _ | Row_nil | Row_cons _ ->
      to_string tp
    | Abstraction _ | Application _ | Universal _ | Recursive _ ->
      to_paren_string tp
  in

  let row_to_string row =
    let fields, rest = row_to_list row in
    let field_to_string (id, tp) =
      Printf.sprintf "%s : %s" (Id.to_string id) (to_string tp)
    in
    let fields_str =
      String.concat "; " @@ List.map field_to_string fields
    in
    match rest with
      | None ->
        fields_str
      | Some tp ->
        if String.length fields_str > 0 then
          Printf.sprintf "%s | %s" fields_str (to_string tp)
        else
          Printf.sprintf "%s" (to_string tp)
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
    | Application (Variable id, row) when id = Id.rcrd ->
      Printf.sprintf "{%s}" (row_to_string row)
    | Application (Variable id, row) when id = Id.vrnt ->
      Printf.sprintf "[%s]" (row_to_string row)
    | Application (fn, arg) ->
      let fn_to_string tp = match tp with
        | Variable _ | Application _ | Row_nil | Row_cons _ ->
          to_string tp
        | Abstraction _ | Universal _ | Recursive _ ->
          to_paren_string tp
      in
      Printf.sprintf "%s %s" (fn_to_string fn) (arg_to_string arg)
    | Universal (quant, kn, body) ->
      Printf.sprintf "forall %s :: %s . %s"
        (Id.to_string quant)
        (Kind.to_string kn)
        (to_string body)
    | Recursive (quant, kn, body) ->
      Printf.sprintf "mu %s :: %s . %s"
        (Id.to_string quant)
        (Kind.to_string kn)
        (to_string body)
    | Row_nil | Row_cons _ ->
      Printf.sprintf "<%s>" (row_to_string tp)

(* Constructors *)

let abs' args body =
  List.fold_right (fun (arg, kn) body -> abs arg kn body) args body

let app' fn args = List.fold_left app fn args

let func arg res = app' (var Id.func) [arg; res]

let func' args res = List.fold_right func args res 

let forall' quants body =
  let forall (quant, kn) body = forall quant kn body in
  List.fold_right forall quants body

let mu' quants body =
  List.fold_right (fun (q, kn) body -> mu q kn body) quants body

let rcrd fields rest = app (var Id.rcrd) (row_of_list fields rest)

let vrnt cases rest = app (var Id.vrnt) (row_of_list cases rest)

let row = row_of_list

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

let get_mu tp = match tp with
  | Recursive (quant, kn, body) -> quant, kn, body
  | _ -> invalid_arg "Type.get_mu: expected recursive"

let get_mu' tp =
  let rec get_mu acc tp = match tp with
    | Recursive (quant, kn, body) -> get_mu ((quant, kn) :: acc) body
    | _ -> acc, tp
  in
  get_mu [] tp

let get_rcrd tp = match tp with
  | Application (Variable id, row) when id = Id.rcrd ->
    row_to_list row
  | _ -> invalid_arg "Type.get_rcrd: expected record"

let get_vrnt tp = match tp with
  | Application (Variable id, row) when id = Id.vrnt ->
    row_to_list row
  | _ -> invalid_arg "Type.get_vrnt: expected variant"
