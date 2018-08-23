module Id = Identifier
module Misc = Miscellaneous

type t =
  | Variable of Id.t
  | Function of t * t

(* Exceptions and errors *)

let error : string -> 'a = fun msg -> failwith msg

exception Occurs of Id.t * t

exception Cannot_unify of t * t

let raise_occurs : Id.t -> t -> 'a = fun id tp ->
  raise @@ Occurs (id, tp)

let raise_unify : t -> t -> 'a = fun tp1 tp2 ->
  raise @@ Cannot_unify (tp1, tp2)

(* Constructors *)

let var id = Variable id

let func arg res = Function (arg, res)

let func' args res =
  List.fold_left (fun res arg -> func arg res) res (List.rev args)

(* Inference *)

module Substitution : sig

  type s

  val identity : s

  val extend : Id.t -> t -> s -> s

  val apply : t -> s -> t

end = struct

  type nonrec s = t Id.Map.t

  let identity = Id.Map.empty

  let singleton : Id.t -> t -> s = Id.Map.singleton

  let rec apply : s -> t -> t = fun sub tp -> match tp with
    | Variable id -> Id.Map.find_default tp id sub
    | Function (arg, res) -> func (apply sub arg) (apply sub res)

  let extend id tp sub =
    Id.Map.add id tp @@ Id.Map.map (apply @@ singleton id tp) sub

  let apply tp sub =
    let tp' = apply sub tp in
    assert (tp' = apply sub tp');
    tp'

end

module Rank : sig

  val inc : unit -> unit
  val dec : unit -> unit

  val is_mono : Id.t -> bool
  val is_poly : Id.t -> bool

  val register : Id.t -> unit
  val update : Id.t -> Id.t -> unit
  val make_poly : Id.t -> unit

end = struct

  let init = 0

  let poly = -2

  let curr, inc, dec =
    let rank : int ref = ref init in
    (fun () -> !rank), (fun () -> incr rank), (fun () -> decr rank)

  let get, set =
    let ht = Hashtbl.create 2048 in
    Hashtbl.find ht, Hashtbl.replace ht

  let is_mono id = get id >= init

  let is_poly id = get id = poly

  let register id = set id @@ curr ()

  let update id1 id2 = set id1 @@ min (get id1) (get id2)

  let make_poly id = if get id > curr () then set id poly

end

(*
  [get_quants tp] computes the set polymorphic type variables appearing
  as subtypes of [tp].
 *)
let get_quants : t -> Id.t list = fun tp ->
  let contains id = List.exists (fun id' -> id = id') in
  let rec get_quants acc tp = match tp with
    | Variable id ->
      if Rank.is_poly id && not @@ contains id acc then
        id :: acc
      else
        acc
    | Function (arg, res) ->
      get_quants (get_quants acc arg) res
  in
  List.rev @@ get_quants [] tp

let unify sub tp1 tp2 =

  let module Sub = Substitution in

  let rec occurs id tp = match tp with
    | Variable id' -> id = id'
    | Function (arg, res) -> occurs id arg || occurs id res
  in

  let rec update_ranks id tp = match tp with
    | Variable id' ->
      Rank.update id' id
    | Function (arg, res) ->
      update_ranks id arg;
      update_ranks id res
  in

  let rec unify sub tp1 tp2 =
    let tp1' = Sub.apply tp1 sub in
    let tp2' = Sub.apply tp2 sub in
    match tp1', tp2' with
      | Variable id, _ when not (Rank.is_mono id) ->
        raise_unify tp1 tp2
      | _, Variable id when not (Rank.is_mono id) ->
        raise_unify tp1 tp2
      | Variable id1, Variable id2 when id1 = id2->
        sub
      | Variable id, _ ->
        if occurs id tp2' then raise_occurs id tp2';
        update_ranks id tp2';
        Sub.extend id tp2' sub
      | _, Variable id ->
        if occurs id tp1' then raise_occurs id tp1';
        update_ranks id tp1';
        Sub.extend id tp1' sub
      | Function (arg1, res1), Function (arg2, res2) ->
        unify (unify sub arg1 arg2) res1 res2
  in

  let sub' = unify sub tp1 tp2 in
  assert (Sub.apply tp1 sub' = Sub.apply tp2 sub');
  sub'

let register tp = match tp with
  | Variable id -> Rank.register id
  | _ -> error "Type.register: expected variable"

let gen_enter () = Rank.inc ()

let gen_exit tp =

  Rank.dec ();

  let rec gen tp = match tp with
    | Variable id ->
      Rank.make_poly id
    | Function (arg, res) ->
      gen arg;
      gen res
  in

  gen tp;
  get_quants tp

let inst tp =

  let quants = get_quants tp in
  let vars = List.map (fun _ -> var @@ Id.fresh_upper ()) quants in
  List.iter register vars;
  let env = Id.Map.of_list @@ List.combine quants vars in

  let rec inst tp = match tp with
    | Variable id -> Id.Map.find_default tp id env
    | Function (arg, res) -> func (inst arg) (inst res)
  in

  vars, inst tp

(* Utilities *)

let to_intl_repr tp =

  let module IR = Universal_types.Type in
  let rec to_ir tp = match tp with
    | Variable id -> IR.var id
    | Function (arg, res) -> IR.func (to_ir arg) (to_ir res)
  in

  IR.forall' (get_quants tp) (to_ir tp)

(*
  NOTE: [simplify] does not register the new variables that it creates
  with [Rank], so [simplify]'d types cannot be used with inference
  functions.
 *)
let simplify tp =

  let fresh =
    let cntr = ref (-1) in
    fun () ->
      incr cntr;
      Id.of_string @@ Misc.int_to_upper !cntr
  in

  let simplify_id =
    let env = Hashtbl.create 1024 in
    fun id ->
      try Hashtbl.find env id with
        | Not_found ->
          let id' = fresh () in
          Hashtbl.add env id id';
          id'
  in

  let rec simplify tp = match tp with
    | Variable id ->
      var @@ simplify_id id
    | Function (arg, res) ->
      let arg' = simplify arg in
      let res' = simplify res in
      func arg' res'
  in

  simplify tp

let to_string ?no_simp ?show_quants tp =

  let rec to_string tp =
    let to_paren_string tp = Printf.sprintf "(%s)" (to_string tp) in
    match tp with
      | Variable id ->
          Id.to_string id
      | Function (arg, res) ->
        let arg_to_string tp = match tp with
          | Variable _ -> to_string tp
          | Function _ -> to_paren_string tp
        in
        Printf.sprintf "%s -> %s" (arg_to_string arg) (to_string res)
  in

  let tp' = if no_simp = None then simplify tp else tp in
  if show_quants = None then
    to_string tp'
  else
    let quants = get_quants tp' in
    Printf.sprintf "forall %s . %s"
      (String.concat " " @@ List.map Id.to_string quants)
      (to_string tp')
