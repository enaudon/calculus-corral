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

let push x {size; data} = {size = size + 1; data = Map.add size x data}

let get i {size; data} =
  if i < 0 || i >= size then
    invalid_arg "Stack.get";
  Map.find i data

let peek s =
  if s.size <= 0 then
    invalid_arg "Stack.peek";
  get (s.size - 1) s

let pop {size; data} =
  if size <= 0 then
    invalid_arg "Stack.pop";
  let size' = size - 1 in
  {size = size'; data = Map.remove size' data}

let update i x {size; data} =
  if i < 0 || i >= size then
    invalid_arg "Stack.update";
  {size; data = Map.add i x data}
