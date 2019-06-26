module Id = Identifier
module Kind_env = Kind.Environment
module Misc = Miscellaneous

type morph = Mono | Poly

type t =
  | Inference_variable of morph * Id.t
  | Variable of Id.t
  | Abstraction of Id.t * Kind.t * t
  | Application of t * t
  | Universal of Id.t * Kind.t * t

module Env = Type_environment.Make (struct
  type value = t
  let initial_types = []
  let initial_terms = []
end)

(* Exceptions *)

exception Occurs of Id.t * t

exception Cannot_unify of t * t

(* Internal utilities *)

let error : string -> string -> 'a = fun fn_name msg ->
  failwith @@ Printf.sprintf "%s.%s: %s" __MODULE__ fn_name msg

let raise_poly : string -> 'a = fun fn_name ->
  invalid_arg @@
    Printf.sprintf
      "%s.%s: unexpected polymorphic type"
      __MODULE__
      fn_name

let raise_abs : string -> 'a = fun fn_name ->
  invalid_arg @@
    Printf.sprintf
      "%s.%s: unexpected type abstraction"
      __MODULE__
      fn_name

let var_to_string : t -> string = fun tv -> match tv with
  | Inference_variable (_, id) -> "'" ^ Id.to_string id
  | Variable id -> Id.to_string id
  | _ -> error "var_to_string" "expected variable"

(* Constructors *)

let inf_var : morph -> Id.t -> t = fun morph id ->
  Inference_variable (morph, id)

let var id = Variable id

let abs arg kn body = Abstraction (arg, kn, body)

let app : t -> t -> t = fun fn arg -> Application (fn, arg)

let forall : Id.t -> Kind.t -> t -> t = fun quant kn body ->
  Universal (quant, kn, body)

let forall' : (Id.t * Kind.t) list -> t -> t = fun quants body ->
  let forall (quant, kn) body = forall quant kn body in
  List.fold_right forall quants body

(* Destructors *)

let get_forall' : t -> (Identifier.t * Kind.t) list * t = fun tp ->
  let rec get_forall acc tp = match tp with
    | Universal (quant, kn, body) ->
      get_forall ((quant, kn) :: acc) body
    | _ ->
      acc, tp
  in
  let quants, tp = get_forall [] tp in
  List.rev quants, tp

(* Reduction *)

(**
  [subst tp id tp'] replaces occurences of [id] in [tp] with [tp'].

  [subst] avoids name capture by renaming binders in [tp] to follow the
  Barendregt convention--i.e. the names of bound variable are chosen
  distinct from those of free variables.
 *)
let rec subst : Id.Set.t -> Env.t -> t -> t = fun fvs sub tp ->
  match tp with
    | Inference_variable _ ->
      tp
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

let rec beta_reduce env tp =

  let subst env tp id tp' =
    let fvs = Id.Set.of_list @@ Env.Type.keys env in
    subst fvs (Env.Type.singleton id tp') tp
  in

  match tp with
    | Variable id ->
      Env.Type.find_default tp id env
    | Application (fn, act_arg) ->
      begin match beta_reduce env fn with
        | Abstraction (fml_arg, _, body) ->
          subst env body fml_arg act_arg
        | _ ->
          tp
      end
    | _ ->
      tp

(* Inference *)

module Inferencer : sig

  type state
  val make_state : Kind_env.t -> Env.t -> state
  val register : ?rigid : unit -> state -> t -> Kind.t -> state
  val to_kind : state -> t -> Kind.t
  val unify : state -> t -> t -> state
  val gen_enter : state -> state
  val gen_exit : state -> t -> state * Kind.t Id.Map.t * t
  val inst : state -> t -> state * t list * t
  val apply : state -> t -> t

end = struct

  module IVE = Inference_variable_environment

  type sub = t Id.Map.t

  type rigidity =
    | Flexible
    | Rigid

  type state = {
    sub : sub ;
    pools : (Kind.t * rigidity) IVE.t ;
    kind_env : Kind_env.t ;
    type_env : Env.t ;
  }

  module Sub : sig

    val identity : sub
    val extend : Id.t -> t -> state -> state
    val apply : t -> state -> t

  end = struct

    let identity = Id.Map.empty

    let singleton : Id.t -> t -> sub = Id.Map.singleton

    let rec apply tp sub = match tp with
      | Inference_variable (_, id) ->
        Id.Map.find_default tp id sub
      | Variable _ ->
        tp
      | Abstraction (arg, kn, body) ->
        abs arg kn @@ apply body sub
      | Application (fn, arg) ->
        app (apply fn sub) (apply arg sub)
      | Universal (quant, kn, body) ->
        forall quant kn @@ apply body sub

    let extend id tp state =
      let apply tp' = apply tp' @@ singleton id tp in
      { state with
        sub = Id.Map.add id tp @@ Id.Map.map apply state.sub }

    let apply tp state =
      let tp' = apply tp state.sub in
      assert (tp' = apply tp' state.sub);
      tp'

  end

  module Pools : sig

    val push : state -> state
    val peek : state -> Kind.t Id.Map.t
    val pop : state -> state
    val insert : Id.t -> Kind.t -> bool -> state -> state
    val remove : Id.t -> state -> state
    val update : Id.t -> Id.t -> state -> state
    val get_kind : Id.t -> state -> Kind.t
    val is_rigid : Id.t -> state -> bool

  end = struct

    let push state = {state with pools = IVE.push state.pools}

    let peek state = Id.Map.map fst @@ IVE.peek state.pools

    let pop state = {state with pools = IVE.pop state.pools}

    let insert id kn is_rigid state =
      let rigidity = if is_rigid then Rigid else Flexible in
      {state with pools = IVE.insert id (kn, rigidity) state.pools}

    let remove id state =
      {state with pools = IVE.remove id state.pools}

    let update id1 id2 state =
      {state with pools = IVE.update id1 id2 state.pools}

    let get_kind id state = fst @@ IVE.find id state.pools

    let is_rigid id state = match snd @@ IVE.find id state.pools with
      | Flexible -> false
      | Rigid -> true

  end

  let raise_occurs : Id.t -> t -> 'a = fun id tp ->
    raise @@ Occurs (id, tp)

  let raise_unify : t -> t -> 'a = fun tp1 tp2 ->
    raise @@ Cannot_unify (tp1, tp2)

  let make_state kind_env type_env =
    {sub = Sub.identity; pools = IVE.empty; kind_env; type_env}

  let add_kind id kn state =
    {state with kind_env = Kind_env.add id kn state.kind_env}

  let get_kind id state =
    Kind_env.find id state.kind_env

  let register ?rigid state tp kn = match tp with
    | Inference_variable (_, id) ->
      Pools.insert id kn (rigid <> None) state
    | _ ->
      error "register" "expected variable"

  (* Kinding *)

  let to_kind state tp =

    let undefined_id tp =
      error "to_kind" @@
        Printf.sprintf "undefined identifier '%s'" (var_to_string tp)
    in

    let rec to_kind state tp = match tp with
      | Inference_variable (Mono, id) ->
        begin try
          Pools.get_kind id state
        with Id.Unbound _ ->
          undefined_id tp
        end
      | Inference_variable (Poly, id) | Variable id ->
        begin try
          get_kind id state
        with Id.Unbound _ ->
          undefined_id tp
        end
      | Abstraction (arg, kn, body) ->
        let body_kn = to_kind (add_kind arg kn state) body in
        Kind.oper kn body_kn
      | Application (fn, arg) ->
        let fn_kn = to_kind state fn in
        let fml_arg_kn, res_kn =
          try
            Kind.get_oper fn_kn
          with Invalid_argument _ ->
            error "to_kind" @@
              Printf.sprintf
                "expected function kind; found '%s'"
                (Kind.to_string fn_kn)
        in
        let act_arg_kn = to_kind state arg in
        if Kind.alpha_equivalent act_arg_kn fml_arg_kn then
          res_kn
        else
          error "to_kind" @@
            Printf.sprintf
              "expected kind '%s'; found kind '%s'"
                (Kind.to_string fml_arg_kn)
                (Kind.to_string act_arg_kn)
      | Universal (quant, kn, body) ->
        to_kind (add_kind quant kn state) body
    in

    to_kind state @@ Sub.apply tp state

  (* Typing *)

  let unify state tp1 tp2 =

    let rec occurs : Id.t -> t -> bool = fun id tp -> match tp with
      | Inference_variable (_, id') -> id = id'
      | Variable _ -> false
      | Abstraction _ -> raise_abs "Inferencer.unify.occurs"
      | Application (fn, arg) -> occurs id fn || occurs id arg
      | Universal _ -> raise_poly "Inferencer.unify.occurs"
    in

    let rec update_ranks : state -> Id.t -> t -> state =
        fun state id tp ->
      match tp with
        | Inference_variable (_, id') ->
          Pools.update id' id state
        | Variable _ ->
          state
        | Abstraction _ -> raise_abs "Inferencer.unify.update_ranks"
        | Application (fn, arg) ->
          update_ranks (update_ranks state id fn) id arg
        | Universal _ ->
          raise_poly "Inferencer.unify.update_ranks"
    in

    let merge : state -> Id.t -> t -> state =
        fun state id tp ->
      let state' = update_ranks state id tp in
      Sub.extend id tp @@ Pools.remove id state'
    in

    let rec unify state tp1 tp2 =
      let tp1' = Sub.apply (beta_reduce state.type_env tp1) state in
      let tp2' = Sub.apply (beta_reduce state.type_env tp2) state in
      match tp1', tp2' with

        | _, Inference_variable (Poly, _)
        | Inference_variable (Poly, _), _
        | _, Universal _
        | Universal _, _ ->
          raise_poly "unify";

        | _, Abstraction _ | Abstraction _, _ ->
          raise_abs "Inferencer.unify"

        | Inference_variable (_, id1), Inference_variable (_, id2)
        | Variable id1, Variable id2
            when id1 = id2 ->
          state

        | Inference_variable _, Inference_variable (_, id)
            when Id.is_generated id ->
          if occurs id tp1' then raise_occurs id tp1';
          merge state id tp1'
        | Inference_variable (_, id), Inference_variable _
            when Id.is_generated id ->
          if occurs id tp2' then raise_occurs id tp2';
          merge state id tp2'

        | _, Inference_variable (_, id)
            when not @@ Pools.is_rigid id state ->
          if occurs id tp1' then raise_occurs id tp1';
          merge state id tp1'
        | Inference_variable (_, id), _
            when not @@ Pools.is_rigid id state ->
          if occurs id tp2' then raise_occurs id tp2';
          merge state id tp2'

        | Application (fn1, arg1), Application (fn2, arg2) ->
          unify (unify state fn1 fn2) arg1 arg2

        | _, _ ->
          raise_unify tp1' tp2'

    in

    let kn1 = to_kind state tp1 in
    let kn2 = to_kind state tp2 in
    if not (Kind.alpha_equivalent kn1 kn2) then
      error "unify" @@
        Printf.sprintf
          "expected '%s'; found '%s'"
          (Kind.to_string kn1)
          (Kind.to_string kn2);

    let state' = unify state tp1 tp2 in
    (* TODO: Enable this assertion. *)
    (* assert (Sub.apply tp1 state' = Sub.apply tp2 state'); *)
    state'

  let gen_enter state = Pools.push state

  let gen_exit state tp =

    let free_inf_vars tp =

      let rec free_inf_vars (seen, fvs) tp = match tp with
        | Inference_variable (_, id) when not @@ Id.Set.mem id seen ->
          (Id.Set.add id seen, id :: fvs)
        | Inference_variable _ | Variable _ ->
          (seen, fvs)
        | Abstraction _ ->
          raise_abs "Inferencer.gen_exit.free_vars"
        | Application (fn, arg) ->
          free_inf_vars (free_inf_vars (seen, fvs) fn) arg
        | Universal _ ->
          raise_poly "Inferencer.gen_exit.free_inf_vars"
      in

      tp
        |> free_inf_vars (Id.Set.empty, [])
        |> snd
        |> List.rev

    in

    let rec gen env tp = match tp with
      | Inference_variable (Mono, id) when Id.Map.mem id env ->
        inf_var Poly id
      | Inference_variable (Poly, id) when Id.Map.mem id env ->
        assert false
      | Inference_variable _ | Variable _ ->
        tp
      | Abstraction _ ->
        raise_abs "Inferencer.gen_exit.gen"
      | Application (fn, arg) ->
        app (gen env fn) (gen env arg)
      | Universal _ ->
        raise_poly "Inferencer.gen_exit.gen"
    in

    let tp' = Sub.apply tp state in
    let quant_kns = Pools.peek state in
    let incl =
      free_inf_vars tp'
        |> List.filter (fun id -> Id.Map.mem id quant_kns)
        |> List.map (fun q -> q, Id.Map.find q quant_kns)
    in

    (Pools.pop state, quant_kns, forall' incl @@ gen quant_kns tp')

  let inst state tp =

    let rec inst env tp = match tp with
      | Inference_variable (Mono, id) when Id.Map.mem id env ->
        assert false
      | Inference_variable (Poly, id) ->
        Id.Map.find_default tp id env
      | Inference_variable _ | Variable _ ->
        tp
      | Abstraction _ ->
        raise_abs "Inferencer.inst"
      | Application (fn, arg) ->
        app (inst env fn) (inst env arg)
      | Universal _ ->
        raise_poly "Inferencer.inst"
    in

    let make_var kn (state, tvs) =
      let tv = Id.gen_upper () in
      (Pools.insert tv kn false state, inf_var Mono tv :: tvs)
    in

    let quants, tp' = get_forall' @@ Sub.apply tp state in
    let quant_ids, quant_kns = List.split quants in
    let state', vars = List.fold_right make_var quant_kns (state, []) in
    let env = Id.Map.of_list @@ List.combine quant_ids vars in

    (state', vars, inst env tp')

  let apply state tp = Sub.apply tp state

end

(* Kinding *)

let to_kind env tp =
  (*
    [Env.initial] is ok here, because kinding does not use the type
    environment.
   *)
  Inferencer.to_kind (Inferencer.make_state env Env.initial) tp

(* Utilities *)

let rec to_intl_repr tp =
  let module IR = Type_operators_exp.Type in
  match tp with
    | Inference_variable (_, id) ->
      IR.var id
    | Variable id ->
      IR.var id
    | Abstraction (arg, kn, body) ->
      IR.abs arg (Kind.to_intl_repr kn) (to_intl_repr body)
    | Application (fn, arg) ->
      IR.app (to_intl_repr fn) (to_intl_repr arg)
    | Universal (quant, kn, body) ->
      IR.forall quant (Kind.to_intl_repr kn) (to_intl_repr body)

(*
  NOTE: [simplify] does not register the new variables that it creates
  with [Pools], so [simplify]'d types cannot be used with inference
  functions.
 *)
let simplify tp =

  let fresh =
    let cntr = ref (-1) in
    fun () ->
      incr cntr;
      Id.define @@ Misc.int_to_upper !cntr
  in

  let rec simplify env tp = match tp with
    | Inference_variable (morph, id) when Id.is_generated id ->
      inf_var morph @@ Id.Map.find id env
    | Inference_variable _ | Variable _ ->
      tp
    | Abstraction (arg, kn, body) when Id.is_generated arg ->
      let arg' = fresh () in
      abs arg' kn @@ simplify (Id.Map.add arg arg' env) body
    | Abstraction (arg, kn, body) ->
      abs arg kn @@ simplify env body
    | Application (fn, arg) ->
      let fn' = simplify env fn in
      let arg' = simplify env arg in
      app fn' arg'
    | Universal (quant, kn, body) when Id.is_generated quant ->
      let quant' = fresh () in
      forall quant' kn @@ simplify (Id.Map.add quant quant' env) body
    | Universal (quant, kn, body) ->
      forall quant kn @@ simplify env body
  in

  simplify (Id.Map.empty) tp

let to_string ?no_simp ?show_quants tp =

  let rec to_paren_string tp = Printf.sprintf "(%s)" (to_string tp)

  and arg_to_string tp = match tp with
    | Inference_variable _ | Variable _ -> to_string tp
    | Abstraction _ | Application _ | Universal _ -> to_paren_string tp

  and to_string tp = match tp with
    | Inference_variable _ | Variable _ ->
      var_to_string tp
    | Abstraction (arg, kn, body) ->
      Printf.sprintf "\\%s :: %s . %s"
        (Id.to_string arg)
        (Kind.to_string kn)
        (to_string body)
    | Application (Application (Variable id as tv, arg), res)
        when id = Id.func ->
      Printf.sprintf "%s %s %s"
        (arg_to_string arg)
        (var_to_string tv)
        (to_string res)
    | Application (fn, arg) ->
      Printf.sprintf "%s %s" (to_string fn) (arg_to_string arg)
    | Universal (quant, kn, body) ->
      if show_quants = None then
        to_string body
      else
        Printf.sprintf "forall %s :: %s . %s"
          (Id.to_string quant)
          (Kind.to_string kn)
          (to_string body)
  in

  to_string @@ if no_simp = None then simplify tp else tp

(* Containers *)

module Environment = Env

(* External functions *)

let inf_var id = inf_var Mono id

let abs' = List.fold_right (fun (arg, kn) body -> abs arg kn body)

let app' fn args = List.fold_left app fn args

let func arg res = app (app (var Id.func) arg) res

let func' args res = List.fold_right func args res

let get_quants tp = fst @@ get_forall' tp
