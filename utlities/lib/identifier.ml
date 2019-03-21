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

(*
let define =
  let ht = Hashtbl.create 1024 in
  let get_num str =
    try
      let num = Hashtbl.find ht str in
      Hashtbl.add ht str @@ num + 1;
      num
    with Not_found ->
      Hashtbl.add ht str 1;
      0
  in
  fun str -> Defined (str, get_num str)
*)

(* Constructors *)

let define str = Defined str

let prop = define "*"

let row = define "<>"

let oper = define "=>"

let func = define "->"

let rcrd = define "Π"

let vrnt = define "Σ"

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

(* Utilities *)

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
  (*
    Note that, in this branch, [id1] and [id2] are not necessarily of
    the same case.  That is, [id1] could be [Generated] while [id2] is
    [Defined], or visa-versa.
   *)
  | (id1', id2') :: env' ->
    (id1 = id1' && id2 = id2') ||
      (id1 <> id1' && id2 <> id2' && alpha_equivalent env' id1 id2)
