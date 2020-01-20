type elt = int

type 'a desc =
  (* A set's representative element. The first argument is the set's data, and
     the second is the set's size. *)
  | Repr of 'a * int
  (* A link to another element in the same set. *)
  | Link of elt

type 'a t = 'a desc Vector.t

let empty = Vector.empty

let singleton x ds =
  let elt = Vector.size ds in
  let ds' = Vector.push_back (Repr (x, 0)) ds in
  (ds', elt)

(* [repr a ds] computes the representative element of [a]'s set, and performs
   path compression while traversing the links from [a] to it's representative
   element. *)
let rec repr : elt -> 'a t -> 'a t * elt =
 fun a ds ->
   match Vector.get a ds with
     | Link b ->
       let ds, c = repr b ds in
       if c <> b then
         (Vector.set a (Vector.get b ds) ds, c)
       else
         (ds, c)
     | Repr _ ->
       (ds, a)

let rec find a ds =
  match Vector.get a ds with
    | Repr (x, _) ->
      (ds, x)
    | Link _ ->
      let ds', a' = repr a ds in
      find a' ds'

(* [merge a b] merges the sets containing [a] and [b]. The smaller set is always
   merged into the larger one, but [a]'s representative element is kept. *)
let rec merge a b ds =
  match (Vector.get a ds, Vector.get b ds) with
    | Repr (x, a_size), Repr (_, b_size) ->
      let size = a_size + b_size in
      if a_size >= b_size then
        ds |> Vector.set b @@ Link a |> Vector.set a @@ Repr (x, size)
      else
        ds |> Vector.set a @@ Link b |> Vector.set b @@ Repr (x, size)
    | Link _, _ ->
      let ds', a' = repr a ds in
      merge a' b ds'
    | _, Link _ ->
      let ds', b' = repr b ds in
      merge a b' ds'

let rec update a x ds =
  match Vector.get a ds with
    | Repr (_, size) ->
      Vector.set a (Repr (x, size)) ds
    | Link _ ->
      let ds', a' = repr a ds in
      update a' x ds'
