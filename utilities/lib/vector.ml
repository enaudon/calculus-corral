(* TODO: Replace this with a more efficient data-structure. *)
module Map = Map.Make (struct
  type t = int

  let compare = Stdlib.compare
end)

type 'a t =
  { size : int;
    data : 'a Map.t }

let empty = {size = 0; data = Map.empty}

let size s = s.size

let get i {size; data} =
  if i < 0 || i >= size then
    invalid_arg "Vector.get";
  Map.find i data

let set i x {size; data} =
  if i < 0 || i >= size then
    invalid_arg "Vector.set";
  {size; data = Map.add i x data}

let push_back x v = set v.size x {v with size = v.size + 1}

let peek_back v = get (size v - 1) v

let pop_back {size; data} =
  if size <= 0 then
    invalid_arg "Vector.pop_back";
  let size' = size - 1 in
  {size = size'; data = Map.remove size' data}
