type t = string

exception Unbound of t

module OrdId = struct
  type nonrec t = t
  let compare = Pervasives.compare
end

module Set = struct

  include Set.Make (OrdId)

  let del = remove

end

module Map = struct

  include Map.Make (OrdId)

  let of_list l =
    List.fold_left (fun map (id, v) -> add id v map) empty l

  let del = remove

  let find_default x id map =
    try find id map with
      | Not_found -> x

  let find id map =
    try find id map with
      | Not_found -> raise @@ Unbound id

  let keys map = List.map fst @@ bindings map

  let values map = List.map snd @@ bindings map

end

let fresh_lower, reset_lower =
  let cntr = ref (-1) in
  let fresh () =
    incr cntr;
    Miscellaneous.int_to_lower !cntr
  in
  let reset () = cntr := -1 in
  fresh, reset

let fresh_upper, reset_upper =
  let cntr = ref (-1) in
  let fresh () =
    incr cntr;
    Miscellaneous.int_to_upper !cntr
  in
  let reset () = cntr := -1 in
  fresh, reset

let reset () =
  reset_lower ();
  reset_upper ()

let of_string str = str

let to_string id = id

let rec alpha_equivalent env id1 id2 = match env with
  | [] -> id1 = id2
  | (id1', id2') :: env' ->
    (id1 = id1' && id2 = id2') ||
      (id1 <> id1' && id2 <> id2' && alpha_equivalent env' id1 id2)
