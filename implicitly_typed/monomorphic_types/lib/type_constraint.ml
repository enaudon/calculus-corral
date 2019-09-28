module Id = Identifier
module Loc = Location
module Sub = Type.Substitution

type co =
  | Variable_equality of Id.t * Type.t
  | Type_equality of Type.t * Type.t
  | Conjunction of co * co
  | Existential of Type.t * co
  | Binding of Id.t * Type.t * co
  | Localized of Loc.t * co

type 'a t = co * (Sub.s -> 'a)

(* Internal helpers *)

let error : Loc.t -> string -> string -> 'a =
 fun loc fn_name msg ->
   failwith
   @@ Printf.sprintf "%s %s.%s: %s" (Loc.to_string loc) __MODULE__ fn_name msg

let fresh_inf_var () = Type.inf_var @@ Id.gen_upper ()

let loc_wrap : Loc.t option -> co -> co =
 fun p ->
   match p with Some p -> fun c -> Localized (p, c) | None -> fun c -> c

(* Solving *)

let solve env (c, k) =
  let module Type_env = Type.Environment in
  let rec solve env sub c =
    match c with
      | Variable_equality (id, tp) ->
        Type.unify sub tp @@ Type_env.Term.find id env
      | Type_equality (lhs, rhs) ->
        Type.unify sub lhs rhs
      | Conjunction (lhs, rhs) ->
        solve env (solve env sub lhs) rhs
      | Existential (_, c) ->
        solve env sub c
      | Binding (id, tp, c) ->
        solve (Type_env.Term.add id tp env) sub c
      | Localized (loc, c) ->
        try solve env sub c with
          | Type.Occurs (id, tp) ->
            error loc "solve"
            @@ Printf.sprintf
                 "type variable '%s' occurs in '%s'"
                 (Id.to_string id)
                 (Type.to_string ~no_simp:() tp)
          | Id.Unbound id ->
            error loc "solve"
            @@ Printf.sprintf "undefined identifier '%s'" (Id.to_string id)
  in
  k @@ solve env Sub.identity c

(* Utilities *)

let to_string ?no_simp (c, _) =
  let type_to_string = Type.to_string ?no_simp in
  let rec to_string c =
    let to_paren_string c = Printf.sprintf "(%s)" (to_string c) in
    match c with
      | Variable_equality (id, tp) ->
        Printf.sprintf "%s = %s" (Id.to_string id) (type_to_string tp)
      | Type_equality (lhs, rhs) ->
        Printf.sprintf "%s = %s" (type_to_string lhs) (type_to_string rhs)
      | Conjunction (lhs, rhs) ->
        let rec to_string' c =
          match c with
            | Variable_equality _ | Type_equality _ | Conjunction _ ->
              to_string c
            | Existential _ | Binding _ ->
              to_paren_string c
            | Localized (_, c) ->
              to_string' c
        in
        Printf.sprintf "%s & %s" (to_string' lhs) (to_string' rhs)
      | Existential (tv, c) ->
        Printf.sprintf "exists %s . %s" (type_to_string tv) (to_string c)
      | Binding (id, tp, c) ->
        Printf.sprintf
          "def %s = %s in %s"
          (Id.to_string id)
          (type_to_string tp)
          (to_string c)
      | Localized (_, c) ->
        to_string c
  in
  to_string c

(* Constructors *)

let map f (c, k) = (c, fun sub -> f @@ k sub)

let var_eq ?loc id tp = (loc_wrap loc @@ Variable_equality (id, tp), fun _ -> ())

let type_eq ?loc lhs rhs =
  (loc_wrap loc @@ Type_equality (lhs, rhs), fun _ -> ())

let conj ?loc (lhs_c, lhs_k) (rhs_c, rhs_k) =
  (loc_wrap loc @@ Conjunction (lhs_c, rhs_c), fun sub -> (lhs_k sub, rhs_k sub))

let exists ?loc fn =
  let tv = fresh_inf_var () in
  let c, k = fn tv in
  (loc_wrap loc @@ Existential (tv, c), fun sub -> (Sub.apply tv sub, k sub))

let def ?loc id tp (c, k) = (loc_wrap loc @@ Binding (id, tp, c), k)

module Operators = struct
  let ( <$> ) ck f = map f ck
end
