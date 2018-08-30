let default x opt = match opt with
  | None -> x
  | Some x -> x

let fold fn init opt = match opt with
  | None -> init
  | Some x -> fn init x
