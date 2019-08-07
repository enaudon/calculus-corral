module Id = Identifier
module Infer = Type.Inferencer

type t =
  | Type of Type.t
  | Universal of Id.t * Kind.t * t
  | Existential of Id.t * Kind.t * t

let rec infer state an = match an with
  | Type tp ->
    (state, tp)
  | Universal (quant, kn, an) ->
    let tv = Type.inf_var quant in
    let state' = Infer.register ~rigid:() state tv kn in
    infer state' an
  | Existential (quant, kn, an) ->
    let tv = Type.inf_var quant in
    let state' = Infer.register state tv kn in
    infer state' an

let constrain an term_co_fn =

  let rec get_typo an = match an with
    | Type tp -> tp
    | Universal (_, _, an) | Existential (_, _, an) -> get_typo an
  in

  let rec get_universals an = match an with
    | Type _ -> []
    | Universal (quant, kn, an) -> (quant, kn) :: get_universals an
    | Existential (_, _, an) -> get_universals an
  in

  let rec get_existentials an = match an with
    | Type _ -> []
    | Universal (_, _, an) -> get_universals an
    | Existential (quant, kn, an) -> (quant, kn) :: get_existentials an
  in

  let module TC = Type_constraint in
  let open TC.Operators in
  let e_qs = get_existentials an in
  let u_qs = get_universals an in
  let c1, c2 = term_co_fn@@ get_typo an in
  TC.exists_list e_qs (fun _ ->
    TC.conj_left
      (TC.forall_list u_qs (fun _ -> c1) <$> snd)
      (TC.exists_list u_qs (fun _ -> c2) <$> snd)) <$>
    snd

let rec to_string an = match an with
  | Type tp ->
    Type.to_string tp
  | Universal (quant, kn, an) ->
    Printf.sprintf "forall '%s :: %s . %s"
      (Id.to_string quant)
      (Kind.to_string kn)
      (to_string an)
  | Existential (quant, kn, an) ->
    Printf.sprintf "exists '%s :: %s . %s"
      (Id.to_string quant)
      (Kind.to_string kn)
      (to_string an)

let typo tp = Type tp

let forall quant kn body = Universal (quant, kn, body)

let exists quant kn body = Existential (quant, kn, body)
