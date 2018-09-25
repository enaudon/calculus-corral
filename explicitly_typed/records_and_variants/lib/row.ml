module Id = Identifier

type 'a t =
  | Tail
  | Field of Id.t * 'a * 'a t

(* Internal functions *)

let invalid_arg fn_name msg =
  invalid_arg @@ Printf.sprintf "%s.%s: %s" __MODULE__ fn_name msg

let tail : 'a t = Tail

let field : Id.t -> 'a -> 'a t -> 'a t = fun id vl rest ->
  Field (id, vl, rest)

(* External functions *)

let of_list fields =
  List.fold_right (fun (id, vl) acc -> field id vl acc) fields Tail

let rec to_list row = match row with
  | Tail -> []
  | Field (id, vl, rest) -> (id, vl) :: to_list rest

let rec iter fn row = match row with
  | Tail -> ()
  | Field (id, vl, rest) -> fn id vl; iter fn rest

let rec map fn row = match row with
  | Tail -> tail
  | Field (id, vl, rest) -> field id (fn vl) (map fn rest)

let rec fold fn row acc = match row with
  | Tail -> acc
  | Field (id, vl, rest) -> fold fn rest @@ fn id vl acc

(* TODO: Make this order-agnostic. *)
let rec for_all2 pred row1 row2 = match row1, row2 with
  | Tail, Tail -> true
  | Tail, Field _ | Field _, Tail ->
    invalid_arg "for_all2" "expected rows of the same length"
  | Field (id1, vl1, rest1), Field (id2, vl2, rest2) ->
    if pred id1 vl1 id2 vl2 then
      for_all2 pred rest1 rest2
    else
      false
