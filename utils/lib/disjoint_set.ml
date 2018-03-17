type 'a repr = {
  repr : 'a ;
  weight : int ;
} 

type 'a t = 'a desc ref

and 'a desc =
  | Repr of 'a repr
  | Link of 'a t

let singleton repr = ref @@ Repr { repr; weight = 1 }

(*
  [repr x] computes the representative element of the set containing the
  element [x] and points [x] directly at this element.  In the process
  of traversing the links from [x] to it's representative element,
  [repr] performs path compression, pointing each link along the path
  directly to the representative element.
 *)
let rec repr : 'a t -> 'a t = fun x -> match !x with
  | Link y ->
    let z = repr y in
    if z != y then x := !z;
    z
  | Repr _ ->
    x

let rec find x = match !x with
  | Repr { repr; _ } -> repr
  | Link _ -> find @@ repr x

(*
  [merge x y] merges the sets containing [x] and [y], while keeping the
  representative element of [x]'s set.  The smaller set--i.e. the one
  with the lower weight--is always merged into the larger one.  This
  ensures that the lengths of the paths in the resulting set remain
  logarithmic in terms of the size of the set.
 *)
let rec merge x y = match !x, !y with
  | Repr { repr; weight = x_weight }, Repr { weight = y_weight; _ } ->
    let weight = x_weight + y_weight in
    if x_weight >= y_weight then (
      y := Link x;
      x := Repr { repr; weight }
    ) else (
      x := Link y;
      y := Repr { repr; weight }
    )
  | Link _, _ ->
    merge (repr x) y
  | _, Link _ ->
    merge x (repr y)

let rec update x data = match !x with
  | Repr { weight; _ } -> x := Repr { repr = data; weight }
  | Link _ -> update (repr x) data
