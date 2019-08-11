module Id = Identifier
module Kind_env = Kind.Environment
module Misc = Miscellaneous

type morph = Mono | Poly

type t =
  | Inference_variable of morph * Id.t
  | Variable of Id.t
  | Application of t * t
  | Universal of Id.t * Kind.t * t

module Environment = Type_environment.Make (struct
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

let invalid_arg : string -> string -> 'a = fun fn_name msg ->
  invalid_arg @@ Printf.sprintf "%s.%s: %s" __MODULE__ fn_name msg

let raise_mono : string -> 'a = fun fn_name ->
  invalid_arg fn_name "unexpected monomorphic type"

let raise_poly : string -> 'a = fun fn_name ->
  invalid_arg fn_name "unexpected polymorphic type"

let var_to_string : t -> string = fun tv -> match tv with
  | Inference_variable (_, id) -> "'" ^ Id.to_string id
  | Variable id -> Id.to_string id
  | _ -> error "var_to_string" "expected variable"

(* Constructors *)

let inf_var : morph -> Id.t -> t = fun morph id ->
  Inference_variable (morph, id)

let var id = Variable id

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

(* Inference *)

module Inferencer : sig

  type state
  val make_state : Kind.Environment.t -> state
  val register : state -> t -> Kind.t -> state
  val to_kind : state -> t -> Kind.t
  val unify : state -> t -> t -> state
  val gen_enter : state -> state
  val gen_exit : state -> t -> state * Kind.t Id.Map.t * t
  val inst : state -> t -> state * t list * t
  val apply : state -> t -> t

end = struct

  module IVE = Inference_variable_environment

  type sub = t Id.Map.t

  type state = {
    sub : sub;
    pools : Kind.t IVE.t;
    kind_env : Kind_env.t;
  }

  module Sub : sig

    val identity : sub
    val extend : Id.t -> t -> state -> state
    val apply : t -> state -> t

  end = struct

    let identity = Id.Map.empty

    let singleton : Id.t -> t -> sub = Id.Map.singleton

    let rec apply tp sub = match tp with
      | Inference_variable (_, id) -> Id.Map.find_default tp id sub
      | Variable _ -> tp
      | Application (fn, arg) -> app (apply fn sub) (apply arg sub)
      | Universal (quant, kn, body) -> forall quant kn @@ apply body sub

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
    val insert : Id.t -> Kind.t -> state -> state
    val remove : Id.t -> state -> state
    val update : Id.t -> Id.t -> state -> state
    val get_kind : Id.t -> state -> Kind.t

  end = struct

    let push state = {state with pools = IVE.push state.pools}

    let peek state = IVE.peek state.pools

    let pop state = {state with pools = IVE.pop state.pools}

    let insert id kn state =
      {state with pools = IVE.insert id kn state.pools}

    let remove id state =
      {state with pools = IVE.remove id state.pools}

    let update id1 id2 state =
      {state with pools = IVE.update id1 id2 state.pools}

    let get_kind id state = IVE.find id state.pools

  end

  let raise_occurs : Id.t -> t -> 'a = fun id tp ->
    raise @@ Occurs (id, tp)

  let raise_unify : t -> t -> 'a = fun tp1 tp2 ->
    raise @@ Cannot_unify (tp1, tp2)

  let make_state kind_env =
    {sub = Sub.identity; pools = IVE.empty; kind_env}

  let add_kind id kn state =
    {state with kind_env = Kind_env.add id kn state.kind_env}

  let get_kind id state =
    Kind_env.find id state.kind_env

  let register state tp kn = match tp with
    | Inference_variable (_, id) -> Pools.insert id kn state
    | _ -> error "register" "expected variable"

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
      let tp1' = Sub.apply tp1 state in
      let tp2' = Sub.apply tp2 state in
      match tp1', tp2' with

        | _, Inference_variable (Poly, _)
        | Inference_variable (Poly, _), _
        | _, Universal _
        | Universal _, _ ->
          raise_poly "Inferencer.unify.unify";

        | Inference_variable (_, id1), Inference_variable (_, id2)
        | Variable id1, Variable id2
            when id1 = id2 ->
          state

        | _, Inference_variable (_, id) ->
          if occurs id tp1' then raise_occurs id tp1';
          merge state id tp1'
        | Inference_variable (_, id), _ ->
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
        raise_poly "Inferencer.gen_exit.gen"
      | Inference_variable _ | Variable _ ->
        tp
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
        raise_mono "Inferencer.inst.inst"
      | Inference_variable (Poly, id) ->
        Id.Map.find_default tp id env
      | Inference_variable _ | Variable _ ->
        tp
      | Application (fn, arg) ->
        app (inst env fn) (inst env arg)
      | Universal _ ->
        raise_poly "Inferencer.inst"
    in

    let make_var kn (state, tvs) =
      let tv = Id.gen_upper () in
      (Pools.insert tv kn state, inf_var Mono tv :: tvs)
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
  Inferencer.to_kind (Inferencer.make_state env) tp

(* Utilities *)

let rec to_intl_repr tp =
  let module IR = Type_operators_exp.Type in
  match tp with
    | Inference_variable (_, id) | Variable id ->
      IR.var id
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
    | Inference_variable (morph, id) ->
      inf_var morph @@ Id.Map.find id env
    | Variable _ ->
      tp
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
    | Application _ | Universal _ -> to_paren_string tp

  and to_string tp = match tp with
    | Inference_variable _ | Variable _ ->
      var_to_string tp
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

(* External functions *)

let inf_var id = inf_var Mono id

let func arg res = app (app (var Id.func) arg) res

let func' args res = List.fold_right func args res

let get_quants tp = fst @@ get_forall' tp
