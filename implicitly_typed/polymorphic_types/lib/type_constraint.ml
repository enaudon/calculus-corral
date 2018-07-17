module Id = Identifier
module Loc = Location

type t =
  | Instance of Id.t * int * Type.t
  | Equality of Type.t * Type.t
  | Conjunction of t * t
  | Existential of Id.t * t
  | Def_binding of Id.t * Type.t * t
  | Let_binding of Id.t * t * Type.t * t
  | Localized of Loc.t * t

(* Internal helpers *)

let error : Loc.t -> string -> 'a = fun loc msg ->
  failwith @@ Printf.sprintf "%s: %s" (Loc.to_string loc) msg

(* Solving *)

let solve rank c =
  let rec solve rank env c = match c with
    | Instance (id, rank, tp) ->
      let _, tp' = Type.inst rank @@ Id.Map.find id env in
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
    | Let_binding (id, lhs, tp, rhs) ->
      solve (rank + 1) env lhs;
      solve rank (Id.Map.add id (Type.gen rank tp) env) rhs
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
  solve rank Id.Map.empty c

(* Utilities *)

let rec to_string ?no_simp =
  let type_to_string = Type.to_string ?no_simp in
  let to_paren_string c = Printf.sprintf "(%s)" (to_string c) in
  fun c -> match c with
    | Instance (id, _, tp) ->
      Printf.sprintf "%s = %s" (Id.to_string id) (type_to_string tp)
    | Equality (lhs, rhs) ->
      Printf.sprintf "%s = %s" (type_to_string lhs) (type_to_string rhs)
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
    | Let_binding (id, lhs, tp, rhs) ->
      Printf.sprintf "let %s = %s[%s] in %s"
        (Id.to_string id)
        (to_string lhs)
        (type_to_string tp)
        (to_string rhs)
    | Localized (_, c) ->
      to_string c

(* Constructors *)

let loc_wrap : Loc.t option -> t -> t = fun p -> match p with
  | Some p -> fun c -> Localized (p, c)
  | None -> fun c -> c

let inst ?loc rank id tp = loc_wrap loc @@ Instance (id, rank, tp)

let equals ?loc lhs rhs = loc_wrap loc @@ Equality (lhs, rhs)

let conj ?loc lhs rhs = loc_wrap loc @@ Conjunction (lhs, rhs)

let exists ?loc fn =
  let id = Id.fresh_upper () in
  loc_wrap loc @@ Existential (id, fn id)

let def ?loc id tp c = loc_wrap loc @@ Def_binding (id, tp, c)

let let_ ?loc id lhs tp rhs =
  loc_wrap loc @@ Let_binding (id, lhs, tp, rhs)
