module Id = Identifier
module Misc = Miscellaneous

type t =
  | Variable of Id.t * int ref
  | Function of t * t

(* Exceptions and errors *)

let error : string -> 'a = fun msg -> failwith msg

exception Occurs of Id.t * t

exception Cannot_unify of t * t

let raise_occurs : Id.t -> t -> 'a = fun id tp ->
  raise @@ Occurs (id, tp)

let raise_unify : t -> t -> 'a = fun tp1 tp2 ->
  raise @@ Cannot_unify (tp1, tp2)

(* Internal utilities *)

let void_rank = -2

(* [poly_rank] is the rank for polymorphic variables. *)
let poly_rank = -1

let top_rank = 0

let var rank id = Variable (id, ref rank)

let func arg res = Function (arg, res)

let func' args res =
  List.fold_left (fun res arg -> func arg res) res (List.rev args)

let get_quants tp =
  let contains id = List.exists (fun id' -> id = id') in
  let rec get_quants acc tp = match tp with
    | Variable (id, rank) ->
      if !rank = poly_rank && not @@ contains id acc then
        id :: acc
      else
        acc
    | Function (arg, res) ->
      get_quants (get_quants acc arg) res
  in
  List.rev @@ get_quants [] tp

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
    | Variable (id, _) -> Id.Map.find_default tp id sub
    | Function (arg, res) -> func (apply sub arg) (apply sub res)

  let extend id tp sub =
    Id.Map.add id tp @@ Id.Map.map (apply @@ singleton id tp) sub

  let apply tp sub =
    let tp' = apply sub tp in
    assert (tp' = apply sub tp');
    tp'

end

let unify sub tp1 tp2 =

  let module Sub = Substitution in

  let rec occurs id tp = match tp with
    | Variable (id', _) -> id = id'
    | Function (arg, res) -> occurs id arg || occurs id res
  in

  let rec update_ranks rank tp = match tp with
    | Variable (_, rank') ->
      rank' := min rank !rank'
    | Function (arg, res) ->
      update_ranks rank arg;
      update_ranks rank res
  in

  let rec unify sub tp1 tp2 =
    let tp1' = Sub.apply tp1 sub in
    let tp2' = Sub.apply tp2 sub in
    match tp1', tp2' with
      | Variable (_, rank), _
          when !rank = poly_rank || !rank = void_rank->
        raise_unify tp1 tp2
      | _, Variable (_, rank)
          when !rank = poly_rank || !rank = void_rank->
        raise_unify tp1 tp2
      | Variable (id1, _), Variable (id2, _) when id1 = id2->
        sub
      | Variable (id, rank), _ ->
        if occurs id tp2' then raise_occurs id tp2';
        update_ranks !rank tp2';
        Sub.extend id tp2' sub
      | _, Variable (id, rank) ->
        if occurs id tp1' then raise_occurs id tp1';
        update_ranks !rank tp1';
        Sub.extend id tp1' sub
      | Function (arg1, res1), Function (arg2, res2) ->
        unify (unify sub arg1 arg2) res1 res2
    in

  let sub' = unify sub tp1 tp2 in
  assert (Sub.apply tp1 sub' = Sub.apply tp2 sub');
  sub'

let gen rank tp =

  let rec gen tp = match tp with
    | Variable (id, rank') ->
      if !rank' > rank then
        var poly_rank id
      else
        tp
    | Function (arg, res) ->
      func (gen arg) (gen res)
  in

  let tp' = gen tp in
  get_quants tp', tp'

let inst rank tp =

  let quants = get_quants tp in
  let vars = List.map (fun _ -> var rank @@ Id.fresh_upper ()) quants in
  let env = Id.Map.of_list @@ List.combine quants vars in

  let rec inst tp = match tp with
    | Variable (id, _) -> Id.Map.find_default tp id env
    | Function (arg, res) -> func (inst arg) (inst res)
  in

  vars, inst tp

(* Utilities *) 

let to_intl_repr tp =

  let module IR = Universal_types.Type in
  let rec to_ir tp = match tp with
    | Variable (id, _) -> IR.var id
    | Function (arg, res) -> IR.func (to_ir arg) (to_ir res)
  in

  IR.forall' (get_quants tp) (to_ir tp)

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
    | Variable (id, rank) ->
      var !rank @@ simplify_id id
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
      | Variable (id, _) ->
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

(* Setters *)

let set_rank rank tp = match tp with
  | Variable (_, rank') -> rank' := rank
  | _ -> error "Type.set_rank: expected variable"
