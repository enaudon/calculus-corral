module Id = Identifier
module Infer = Type.Inferencer

type t =
  | Type of Type.t
  | Universal of Id.t list * Type.t
  | Existential of Id.t list * Type.t

let invalid_arg : string -> string -> 'a = fun fn_name msg ->
  invalid_arg @@ Printf.sprintf "%s.%s: %s" __MODULE__ fn_name msg

let typo tp = Type tp

let forall quants tp = Universal (quants, tp)

let exists quants tp = Existential (quants, tp)

let get_forall an = match an with
  | Universal (quants, tp) -> quants, tp
  | _ -> invalid_arg "get_forall" "expected Universal"

let get_exists an = match an with
  | Existential (quants, tp) -> quants, tp
  | _ -> invalid_arg "get_exists" "expected Existential"

let get_typo an = match an with
  | Type tp -> tp
  | _ -> invalid_arg "get_typo" "expected Type"

let infer state an = match an with
  | Type tp -> state, tp
  | Universal (quants, tp) ->
    let tvs = List.map Type.inf_var quants in
    let register state tp = Infer.register ~rigid:() state tp Kind.prop in
    let state' = List.fold_left register state tvs in
    state', tp
  | Existential (quants, tp) ->
    let tvs = List.map Type.inf_var quants in
    let register state tp = Infer.register state tp Kind.prop in
    let state' = List.fold_left register state tvs in
    state', tp

let constrain an term_co_fn =
  let module TC = Type_constraint in
  match an with
    | Type tp ->
      let c1, c2 = term_co_fn tp in
      TC.conj_left c1 c2
    | Universal (quants, tp) ->
      let c1, c2 = term_co_fn tp in
      let quants' = List.map (fun q -> q, Kind.prop) quants in
      TC.conj_left (TC.forall_list quants' c1) (TC.exists_list quants' c2)
    | Existential (quants, tp) ->
      let c1, c2 = term_co_fn tp in
      let quants' = List.map (fun q -> q, Kind.prop) quants in
      TC.exists_list quants' (TC.conj_left c1 c2)

let to_string an = match an with
  | Type tp ->
    Type.to_string tp
  | Universal (quants, tp) ->
    Printf.sprintf "forall %s . %s"
      (String.concat " " @@ List.map Id.to_string quants)
      (Type.to_string tp)
  | Existential (quants, tp) ->
    Printf.sprintf "exists %s . %s"
      (String.concat " " @@ List.map Id.to_string quants)
      (Type.to_string tp)
