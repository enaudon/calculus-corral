module Id = Identifier
module Loc = Location
module State = Type.State

type co =
  | True
  | Instance of Id.t * Type.t * Type.t list ref
  | Equality of Type.t * Type.t
  | Conjunction of co * co
  | Existential of Type.t * Kind.t * co
  | Def_binding of Id.t * Type.t * co
  | Let_binding of
    Id.t option * Type.t ref * Kind.t * co * co * Kind.t Id.Map.t ref
  | Localized of Loc.t * co

type 'a t = co * (State.s -> 'a)

(* Internal helpers *)

let error : Loc.t -> string -> string -> 'a = fun loc fn_name msg ->
  failwith @@
    Printf.sprintf "%s %s.%s: %s"
      (Loc.to_string loc)
      __MODULE__
      fn_name
      msg

let loc_wrap : Loc.t option -> co -> co = fun p -> match p with
  | Some p -> fun c -> Localized (p, c)
  | None -> fun c -> c

let yield x = True, fun _ -> x

let exists
    : Loc.t option -> Type.t -> Kind.t -> 'a t -> (Type.t * 'a) t
    = fun loc_opt tv kn (c, k) ->
  ( loc_wrap loc_opt @@ Existential (tv, kn, c),
    fun state -> State.apply_solution tv state, k state )

let let_
    : Loc.t option ->
      Id.t option ->
      Kind.t ->
      (Type.t -> 'a t) ->
      ('b t) ->
      (Type.t * Kind.t Identifier.Map.t * 'a * 'b) t
    = fun loc_opt id_opt kn fn (rhs_c, rhs_k) ->
  let tv = Type.var @@ Option.default (Id.gen_upper ()) id_opt in
  let lhs_c, lhs_k = fn tv in
  let tvs_ref = ref Id.Map.empty in
  let tp_ref = ref tv in
  ( loc_wrap loc_opt @@
    Let_binding (id_opt, tp_ref, kn, lhs_c, rhs_c, tvs_ref),
    fun state ->
      let tp = State.apply_solution !tp_ref state in
      tp, !tvs_ref, lhs_k state, rhs_k state )

(* Solving *)

let solve (c, k) =

  let rec solve env state c = match c with
    | True ->
      state
    | Instance (id, tp, tvs_ref) ->
      let state, tvs, tp' = Type.inst state @@ Id.Map.find id env in
      tvs_ref := tvs;
      Type.unify state tp tp'
    | Equality (lhs, rhs) ->
      Type.unify state lhs rhs
    | Conjunction (lhs, rhs) ->
      solve env (solve env state lhs) rhs
    | Existential (tv, kn, c) ->
      solve env (Type.register state tv kn) c
    | Def_binding (id, tp, c) ->
      solve (Id.Map.add id tp env) state c
    | Let_binding (id_opt, tp_ref, kn, lhs, rhs, tvs_ref) ->
      let state = Type.gen_enter state in
      let state = solve env (Type.register state !tp_ref kn) lhs in
      let state, tvs, tp = Type.gen_exit state !tp_ref in
      tp_ref := tp;
      tvs_ref := tvs;
      let fn id env = Id.Map.add id tp env in
      solve (Option.fold fn id_opt env) state rhs
    | Localized (loc, c) ->
      try solve env state c with
        | Type.Occurs (id, tp) ->
          error loc "solve" @@
            Printf.sprintf
              "type variable '%s' occurs in '%s'"
              (Id.to_string id)
              (Type.to_string ~no_simp:() tp)
        | Type.Cannot_unify (tp1, tp2) ->
          error loc "solve" @@
            Printf.sprintf
              "cannot unify '%s' and '%s'"
              (Type.to_string ~no_simp:() tp1)
              (Type.to_string ~no_simp:() tp2)
        | Id.Unbound id ->
            error loc "solve" @@
              Printf.sprintf
                "undefined identifier '%s'"
                (Id.to_string id)
  in

  k @@ solve Id.Map.empty State.initial c

(* Utilities *)

let to_string ?no_simp (c, _) =
  let type_to_string = Type.to_string ?no_simp in
  let rec to_string c =
    let to_paren_string c = Printf.sprintf "(%s)" (to_string c) in
    match c with
      | True ->
        "true"
      | Instance (id, tp, _) ->
        Printf.sprintf "%s = %s" (Id.to_string id) (type_to_string tp)
      | Equality (lhs, rhs) ->
        Printf.sprintf "%s = %s"
          (type_to_string lhs)
          (type_to_string rhs)
      | Conjunction (lhs, rhs) ->
        let rec to_string' c = match c with
          | True | Instance _ | Equality _ | Conjunction _ ->
            to_string c
          | Existential _ | Def_binding _ | Let_binding _ ->
            to_paren_string c
          | Localized (_, c) ->
            to_string' c
        in
        Printf.sprintf "%s & %s" (to_string' lhs) (to_string' rhs)
      | Existential (tv, kn, c) ->
        Printf.sprintf "exists %s :: %s . %s"
          (type_to_string tv)
          (Kind.to_string kn)
          (to_string c)
      | Def_binding (id, tp, c) ->
        Printf.sprintf "def %s = %s in %s"
          (Id.to_string id)
          (type_to_string tp)
          (to_string c)
      | Let_binding (None, tp_ref, kn, lhs, rhs, _) ->
        Printf.sprintf "let %s :: %s . %s in %s"
          (type_to_string !tp_ref)
          (Kind.to_string kn)
          (to_string lhs)
          (to_string rhs)
      | Let_binding (Some id, tp_ref, kn, lhs, rhs, _) ->
        Printf.sprintf "let %s = %s :: %s . %s in %s"
          (Id.to_string id)
          (type_to_string !tp_ref)
          (Kind.to_string kn)
          (to_string lhs)
          (to_string rhs)
      | Localized (_, c) ->
        to_string c
  in
  to_string c

(* Constructors *)

let map f (c, k) = c, fun state -> f @@ k state

let inst ?loc id tp =
  let tvs_ref = ref [] in
  ( loc_wrap loc @@ Instance (id, tp, tvs_ref),
    fun state ->
      List.map (fun tv -> State.apply_solution tv state) !tvs_ref )

let equals ?loc lhs rhs =
  ( loc_wrap loc @@ Equality (lhs, rhs), fun _ -> () )

let conj ?loc (lhs_c, lhs_k) (rhs_c, rhs_k) =
  ( loc_wrap loc @@ Conjunction (lhs_c, rhs_c),
    fun state -> lhs_k state, rhs_k state )

let conj_left ?loc lhs rhs = map fst @@ conj ?loc lhs rhs

let conj_right ?loc lhs rhs = map snd @@ conj ?loc lhs rhs

let conj_list ?loc cs =
  let conj_cons c cs = map (fun (x, xs) -> x :: xs) (conj ?loc c cs) in
  List.fold_right conj_cons cs @@ yield []

let exists_list ?loc kns (fn : Type.t list -> 'a t) =
  let exists_cons (tv, kn) c =
    map (fun (tp, (tps, x)) -> (tp :: tps, x)) @@ exists loc tv kn c
  in
  let tvs =
    List.map (fun kn -> Type.var @@ Id.gen_upper (), kn) kns
  in
  List.fold_right exists_cons tvs @@
    map (fun x -> [], x) (fn @@ List.map fst tvs)

let exists ?loc kn fn =
  let tv = Type.var @@ Id.gen_upper () in
  exists loc tv kn @@ fn tv

let def ?loc id tp (c, k) =
  ( loc_wrap loc @@ Def_binding (id, tp, c), k )

let top ?loc kn fn =
  map
    (fun (tp, tvs, rhs_v, ()) -> tp, tvs, rhs_v)
    (let_ loc None kn fn @@ yield ())

let let_ ?loc id kn fn rhs = let_ loc (Some id) kn fn rhs

module Operators = struct

  let ( <$> ) ck f = map f ck

end
