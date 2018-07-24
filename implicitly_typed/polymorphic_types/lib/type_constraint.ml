module Id = Identifier
module Loc = Location

type co =
  | Instance of Id.t * Type.t * Type.t list ref
  | Equality of Type.t * Type.t
  | Conjunction of co * co
  | Existential of Type.t * co
  | Def_binding of Id.t * Type.t * co
  | Let_binding of Id.t * Type.t ref * co * co
  | Localized of Loc.t * co

type 'a t = co * (unit -> 'a)

(* Internal helpers *)

let error : Loc.t -> string -> 'a = fun loc msg ->
  failwith @@ Printf.sprintf "%s: %s" (Loc.to_string loc) msg

(* Solving *)

let solve rank (c, k) =

  let rec solve rank env c = match c with
    | Instance (id, tp, tvs_ref) ->
      let tvs, tp' = Type.inst rank @@ Id.Map.find id env in
      tvs_ref := tvs;
      Type.unify tp' tp
    | Equality (lhs, rhs) ->
      Type.unify lhs rhs
    | Conjunction (lhs, rhs) ->
      solve rank env lhs;
      solve rank env rhs
    | Existential (tv, c) ->
      Type.set_rank rank tv;
      solve rank env c
    | Def_binding (id, tp, c) ->
      solve rank (Id.Map.add id tp env) c
    | Let_binding (id, tv_ref, lhs, rhs) ->
      Type.set_rank (rank + 1) !tv_ref;
      solve (rank + 1) env lhs;
      let tp = Type.gen rank !tv_ref in
      tv_ref := tp;
      solve rank (Id.Map.add id tp env) rhs
    | Localized (loc, c) ->
      try solve rank env c with
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

  solve rank Id.Map.empty c;
  k ()

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
      | Let_binding (id, tv_ref, lhs, rhs) ->
        Printf.sprintf "let %s = %s[%s] in %s"
          (Id.to_string id)
          (type_to_string !tv_ref)
          (to_string lhs)
          (to_string rhs)
      | Localized (_, c) ->
        to_string c
  in
  to_string c

(* Constructors *)

let loc_wrap : Loc.t option -> co -> co = fun p -> match p with
  | Some p -> fun c -> Localized (p, c)
  | None -> fun c -> c

let map f (c, k) = c, fun () -> f @@ k ()

let inst ?loc id tp =
  let tvs = ref [] in
  loc_wrap loc @@ Instance (id, tp, tvs),
  fun () -> List.map Type.to_intl_repr !tvs

let equals ?loc lhs rhs =
  loc_wrap loc @@ Equality (lhs, rhs), fun () -> ()

let conj ?loc (lhs_c, lhs_k) (rhs_c, rhs_k) =
  loc_wrap loc @@ Conjunction (lhs_c, rhs_c),
  fun () -> lhs_k (), rhs_k ()

let conj_left ?loc lhs rhs = map fst @@ conj ?loc lhs rhs

let conj_right ?loc lhs rhs = map snd @@ conj ?loc lhs rhs

let exists ?loc fn =
  let tv = Type.var 0 @@ Id.fresh_upper () in
  let c, k = fn tv in
  loc_wrap loc @@ Existential (tv, c),
  fun () -> Type.to_intl_repr tv, k ()

let exists' ?loc fn = map snd @@ exists ?loc fn

let def ?loc id tp (c, k) =
  loc_wrap loc @@ Def_binding (id, tp, c), k

let let_ ?loc id fn (rhs_c, rhs_k) =
  let tv = Type.var 0 id in
  let lhs_c, lhs_k = fn tv in
  let tv_ref = ref tv in
  loc_wrap loc @@ Let_binding (id, tv_ref, lhs_c, rhs_c),
  fun () -> Type.to_intl_repr !tv_ref, lhs_k (), rhs_k ()

module Operators = struct

  let ( <$> ) ck f = map f ck

end
