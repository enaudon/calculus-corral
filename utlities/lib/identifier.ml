type t =
  | Generated of string
  | Defined of string

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

let define str = Defined str

let gen_lower, reset_lower =
  let cntr = ref (-1) in
  let gen () =
    incr cntr;
    Generated (Miscellaneous.int_to_lower !cntr)
  in
  let reset () = cntr := -1 in
  gen, reset

let gen_upper, reset_upper =
  let cntr = ref (-1) in
  let gen () =
    incr cntr;
    Generated (Miscellaneous.int_to_upper !cntr)
  in
  let reset () = cntr := -1 in
  gen, reset

let is_defined id = match id with
  | Generated _ -> false
  | Defined _ -> true

let is_generated id = match id with
  | Generated _ -> true
  | Defined _ -> false

let reset () =
  reset_lower ();
  reset_upper ()

let to_string id = match id with
  | Generated str -> str
  | Defined str -> str

let rec alpha_equivalent env id1 id2 = match env with
  | [] ->
    id1 = id2
  | (id1', id2') :: env' ->
    (id1 = id1' && id2 = id2') ||
      (id1 <> id1' && id2 <> id2' && alpha_equivalent env' id1 id2)
