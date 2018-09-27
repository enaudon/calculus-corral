let default x opt = match opt with
  | None -> x
  | Some x -> x

let iter fn opt = match opt with
  | None -> ()
  | Some x -> fn x

let map fn opt = match opt with
  | None -> None
  | Some x -> Some (fn x)

let fold fn opt init = match opt with
  | None -> init
  | Some x -> fn x init
