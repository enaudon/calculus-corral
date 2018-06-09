type 'a t = 'a desc ref

and 'a desc =

  (*
    A set's representative element.  The first argument is the set's
    data, and the second is the set's weight.
   *)
  | Repr of 'a * int

  (* A link to another element in the same set. *)
  | Link of 'a t

let singleton data = ref @@ Repr (data, 1)

(*
  [repr x] computes the representative element of the set containing the
  element [x].  In the process of traversing the links from [x] to it's
  representative element, [repr] performs path compression, pointing
  each link along the path, including [x], directly to the
  representative element.
 *)
let rec repr : 'a t -> 'a t = fun x -> match !x with
  | Link y ->
    let z = repr y in
    if z != y then x := !z;
    z
  | Repr _ ->
    x

let rec find x = match !x with
  | Repr (data, _) -> data
  | Link _ -> find @@ repr x

(*
  [merge x y] merges the sets containing [x] and [y], while keeping the
  representative element of [x]'s set.  The smaller set--i.e. the one
  with the lower weight--is always merged into the larger one.  This
  ensures that the lengths of the paths in the resulting set remain
  logarithmic with respect to the size of the set.
 *)
let rec merge x y = match !x, !y with
  | Repr (data, x_weight), Repr (_, y_weight) ->
    let weight = x_weight + y_weight in
    if x_weight >= y_weight then (
      y := Link x;
      x := Repr (data, weight)
    ) else (
      x := Link y;
      y := Repr (data, weight)
    )
  | Link _, _ ->
    merge (repr x) y
  | _, Link _ ->
    merge x (repr y)

let rec update x data = match !x with
  | Repr (_, weight) -> x := Repr (data, weight)
  | Link _ -> update (repr x) data
