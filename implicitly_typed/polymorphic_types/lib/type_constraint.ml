module Id = Identifier
module Loc = Location
module Sub = Type.Substitution

type co =
  | Instance of Id.t * Type.t * Type.t list ref
  | Equality of Type.t * Type.t
  | Conjunction of co * co
  | Existential of Type.t * co
  | Def_binding of Id.t * Type.t * co
  | Let_binding of
    Id.t * Type.t * co * co * Id.Set.t ref * Id.t list ref
  | Localized of Loc.t * co

type 'a t = co * (Sub.s -> 'a)

(* Internal helpers *)

let error : Loc.t -> string -> 'a = fun loc msg ->
  failwith @@ Printf.sprintf "%s: %s" (Loc.to_string loc) msg

(* Solving *)

let solve (c, k) =

  let rec solve env sub c = match c with
    | Instance (id, tp, tvs_ref) ->
      let tvs, tp' = Type.inst @@ Id.Map.find id env in
      tvs_ref := tvs;
      Type.unify sub tp' tp
    | Equality (lhs, rhs) ->
      Type.unify sub lhs rhs
    | Conjunction (lhs, rhs) ->
      solve env (solve env sub lhs) rhs
    | Existential (tv, c) ->
      Type.register tv;
      solve env sub c
    | Def_binding (id, tp, c) ->
      solve (Id.Map.add id tp env) sub c
    | Let_binding (id, tp, lhs, rhs, tvs_ref, qs_ref) ->
      Type.gen_enter ();
      Type.register tp;
      let sub' = solve env sub lhs in
      let tvs, tp' = Type.gen_exit tp in
      tvs_ref := tvs;
      qs_ref := Type.get_quants tp';
      solve (Id.Map.add id tp' env) sub' rhs
    | Localized (loc, c) ->
      try solve env sub c with
        | Type.Cannot_unify (tp1, tp2) ->
          error loc @@
            Printf.sprintf
              "Unification failed -- '%s' ~ '%s'"
              (Type.to_string ~no_simp:() tp1)
              (Type.to_string ~no_simp:() tp2)
        | Type.Occurs (id, tp) ->
          error loc @@
            Printf.sprintf
              "Occurs check failed -- '%s' occurs in '%s'"
              (Id.to_string id)
              (Type.to_string ~no_simp:() tp)
        | Id.Unbound id ->
          error loc @@
            Printf.sprintf
              "Undefined identifier '%s'\n%!"
              (Id.to_string id)
  in

  let sub = solve Id.Map.empty Sub.identity c in
  sub, k sub

(* Utilities *)

let to_string ?no_simp (c, _) =
  let type_to_string = Type.to_string ?no_simp in
  let rec to_string c =
    let to_paren_string c = Printf.sprintf "(%s)" (to_string c) in
    match c with
      | Instance (id, tp, _) ->
        Printf.sprintf "%s = %s" (Id.to_string id) (type_to_string tp)
      | Equality (lhs, rhs) ->
        Printf.sprintf "%s = %s"
          (type_to_string lhs)
          (type_to_string rhs)
      | Conjunction (lhs, rhs) ->
        let rec to_string' c = match c with
          | Instance _ | Equality _ | Conjunction _ ->
            to_string c
          | Existential _ | Def_binding _ | Let_binding _ ->
            to_paren_string c
          | Localized (_, c) ->
            to_string' c
        in
        Printf.sprintf "%s & %s" (to_string' lhs) (to_string' rhs)
      | Existential (tv, c) ->
        Printf.sprintf "exists %s . %s" (Type.to_string tv) (to_string c)
      | Def_binding (id, tp, c) ->
        Printf.sprintf "def %s = %s in %s"
          (Id.to_string id)
          (type_to_string tp)
          (to_string c)
      | Let_binding (id, tp, lhs, rhs, _, _) ->
        Printf.sprintf "let %s = %s[%s] in %s"
          (Id.to_string id)
          (type_to_string tp)
          (to_string lhs)
          (to_string rhs)
      | Localized (_, c) ->
        to_string c
  in
  to_string c

let type_to_ir sub tp = Type.to_intl_repr @@ Sub.apply tp sub

let loc_wrap : Loc.t option -> co -> co = fun p -> match p with
  | Some p -> fun c -> Localized (p, c)
  | None -> fun c -> c

(* Constructors *)

let map f (c, k) = c, fun sub -> f @@ k sub

let inst ?loc id tp =
  let tvs_ref = ref [] in
  ( loc_wrap loc @@ Instance (id, tp, tvs_ref),
    fun sub -> List.map (type_to_ir sub) !tvs_ref )

let equals ?loc lhs rhs =
  ( loc_wrap loc @@ Equality (lhs, rhs), fun _ -> () )

let conj ?loc (lhs_c, lhs_k) (rhs_c, rhs_k) =
  ( loc_wrap loc @@ Conjunction (lhs_c, rhs_c),
    fun sub -> lhs_k sub, rhs_k sub )

let conj_left ?loc lhs rhs = map fst @@ conj ?loc lhs rhs

let conj_right ?loc lhs rhs = map snd @@ conj ?loc lhs rhs

let exists ?loc fn =
  let tv = Type.var @@ Id.fresh_upper () in
  let c, k = fn tv in
  ( loc_wrap loc @@ Existential (tv, c),
    fun sub -> type_to_ir sub tv, k sub )

let exists' ?loc fn = map snd @@ exists ?loc fn

let def ?loc id tp (c, k) =
  ( loc_wrap loc @@ Def_binding (id, tp, c), k )

let let_ ?loc id fn (rhs_c, rhs_k) =
  let tv = Type.var id in
  let lhs_c, lhs_k = fn tv in
  let tvs_ref = ref Id.Set.empty in
  let qs_ref = ref [] in
  ( loc_wrap loc @@ Let_binding (id, tv, lhs_c, rhs_c, tvs_ref, qs_ref),
    fun sub ->
      type_to_ir sub tv, !tvs_ref, !qs_ref, lhs_k sub, rhs_k sub )

module Operators = struct

  let ( <$> ) ck f = map f ck

end
