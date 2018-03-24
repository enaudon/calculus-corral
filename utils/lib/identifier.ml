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

  let del = remove

  let find_default x id map =
    try find id map with
      | Not_found -> x

  let find id map =
    try find id map with
      | Not_found -> raise @@ Unbound id

end

let fresh, reset =
  let cntr = ref (-1) in
  let fresh () = incr cntr; Printf.sprintf "_%d" !cntr in
  let reset () = cntr := -1 in
  fresh, reset

let of_string str = str

let to_string id = id
