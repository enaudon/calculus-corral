module Id = Identifier
module Loc = Location

type co =
  | Instance of Id.t * Type.t * Type.t list ref
  | Equality of Type.t * Type.t
  | Conjunction of co * co
  | Existential of Id.t * co
  | Def_binding of Id.t * Type.t * co
  | Let_binding of
    Id.t * co * Type.t * Type.t ref * co * Identifier.t list ref
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
    | Existential (_, c) ->
      solve rank env c
    | Def_binding (id, tp, c) ->
      solve rank (Id.Map.add id tp env) c
    | Let_binding (id, lhs, tp, tp_ref, rhs, tvs_ref) ->
      let tp' = Type.gen rank tp in
      tp_ref := tp';
      tvs_ref := Type.get_quants tp';
      solve (rank + 1) env lhs;
      solve rank (Id.Map.add id tp' env) rhs
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
      | Existential (id, c) ->
        Printf.sprintf "exists %s . %s" (Id.to_string id) (to_string c)
      | Def_binding (id, tp, c) ->
        Printf.sprintf "def %s = %s in %s"
          (Id.to_string id)
          (type_to_string tp)
          (to_string c)
      | Let_binding (id, lhs, tp, _, rhs, _) ->
        Printf.sprintf "let %s = %s[%s] in %s"
          (Id.to_string id)
          (to_string lhs)
          (type_to_string tp)
          (to_string rhs)
      | Localized (_, c) ->
        to_string c
  in
  to_string c

(* Constructors *)

let loc_wrap : Loc.t option -> co -> co = fun p -> match p with
  | Some p -> fun c -> Localized (p, c)
  | None -> fun c -> c

let inst ?loc id tp =
  let tvs = ref [] in
  loc_wrap loc @@ Instance (id, tp, tvs),
  fun () -> List.map Type.to_intl_repr !tvs

let equals ?loc lhs rhs =
  loc_wrap loc @@ Equality (lhs, rhs), fun () -> ()

let conj ?loc (lhs_c, lhs_k) (rhs_c, rhs_k) =
  loc_wrap loc @@ Conjunction (lhs_c, rhs_c),
  fun () -> lhs_k (), rhs_k ()

let exists ?loc rank fn =
  let id = Id.fresh_upper () in
  let tp = Type.var rank id in
  let c, k = fn tp in
  loc_wrap loc @@ Existential (id, c),
  fun () -> Type.to_intl_repr tp, k ()

let def ?loc id tp (c, k) =
  loc_wrap loc @@ Def_binding (id, tp, c), k

let let_ ?loc id (lhs_c, lhs_k) tp (rhs_c, rhs_k) =
  let tvs = ref [] in
  let tp' = ref tp in
  loc_wrap loc @@ Let_binding (id, lhs_c, tp, tp', rhs_c, tvs),
  fun () -> Type.to_intl_repr !tp', !tvs, lhs_k (), rhs_k ()

let map f (c, k) = c, fun () -> f @@ k ()

module Operators = struct

  let ( <$> ) ck f = map f ck

end
