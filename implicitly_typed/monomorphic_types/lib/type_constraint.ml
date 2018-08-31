module Id = Identifier
module Loc = Location
module Sub = Type.Substitution

type t =
  | Variable_equality of Id.t * Type.t
  | Type_equality of Type.t * Type.t
  | Conjunction of t * t
  | Existential of Id.t * t
  | Definition of Id.t * Type.t * t
  | Localized of Loc.t * t

(* Internal helpers *)

let error : Loc.t -> string -> string -> 'a = fun loc fn_name msg ->
  failwith @@
    Printf.sprintf "%s %s.%s: %s"
      (Loc.to_string loc)
      __MODULE__
      fn_name
      msg

(* Solving *)

let solve c =

  let rec solve env sub c = match c with
    | Variable_equality (id, tp) ->
      Type.unify sub tp @@ Id.Map.find id env
    | Type_equality (lhs, rhs) ->
      Type.unify sub lhs rhs
    | Conjunction (lhs, rhs) ->
      solve env (solve env sub lhs) rhs
    | Existential (_, c) ->
      solve env sub c
    | Definition (id, tp, c) ->
      solve (Id.Map.add id tp env) sub c
    | Localized (loc, c) ->
      try solve env sub c with
        | Type.Occurs (id, tp) ->
          error loc "solve" @@
            Printf.sprintf
            "type variable '%s' occurs in '%s'"
              (Id.to_string id)
              (Type.to_string ~no_simp:() tp)
        | Id.Unbound id ->
          error loc "solve" @@
            Printf.sprintf "undefined identifier '%s'" (Id.to_string id)
  in

  solve Id.Map.empty Sub.identity c

(* Utilities *)

let rec to_string ?no_simp =
  let type_to_string = Type.to_string ?no_simp in
  let to_paren_string c = Printf.sprintf "(%s)" (to_string c) in
  fun c -> match c with
    | Variable_equality (id, tp) ->
      Printf.sprintf "%s = %s" (Id.to_string id) (type_to_string tp)
    | Type_equality (lhs, rhs) ->
      Printf.sprintf "%s = %s" (type_to_string lhs) (type_to_string rhs)
    | Conjunction (lhs, rhs) ->
      let rec to_string' c = match c with
        | Variable_equality _
        | Type_equality _
        | Conjunction _ ->
          to_string c
        | Existential _
        | Definition _ ->
          to_paren_string c
        | Localized (_, c) ->
          to_string' c
      in
      Printf.sprintf "%s & %s" (to_string' lhs) (to_string' rhs)
    | Existential (id, c) ->
      Printf.sprintf "exists %s . %s" (Id.to_string id) (to_string c)
    | Definition (id, tp, c) ->
      Printf.sprintf "def %s = %s in %s"
        (Id.to_string id)
        (type_to_string tp)
        (to_string c)
    | Localized (_, c) ->
      to_string c

(* Constructors *)

let loc_wrap : Loc.t option -> t -> t = fun p -> match p with
  | Some p -> fun c -> Localized (p, c)
  | None -> fun c -> c

let var_eq ?loc id tp = loc_wrap loc @@ Variable_equality (id, tp)

let type_eq ?loc lhs rhs = loc_wrap loc @@ Type_equality (lhs, rhs)

let conj ?loc lhs rhs = loc_wrap loc @@ Conjunction (lhs, rhs)

let exists ?loc fn =
  let id = Id.fresh_upper () in
  loc_wrap loc @@ Existential (id, fn id)

let def ?loc id tp c = loc_wrap loc @@ Definition (id, tp, c)
