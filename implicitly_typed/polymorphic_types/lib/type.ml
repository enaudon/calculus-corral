module Id = Identifier
module DS = Disjoint_set
module Misc = Miscellaneous

type desc =
  | Variable of Id.t * int ref
  | Function of t * t

and t = desc DS.t

(* Exceptions and errors *)

let error : string -> 'a = fun msg -> failwith msg

exception Occurs of Id.t * t

exception Cannot_unify of t * t

let raise_occurs : Id.t -> t -> 'a = fun id tp ->
  raise @@ Occurs (id, tp)

let raise_unify : t -> t -> 'a = fun tp1 tp2 ->
  raise @@ Cannot_unify (tp1, tp2)

(* Internal utilities *)

let poly_rank = -1

let var : int -> Id.t -> t = fun rank id ->
  DS.singleton @@ Variable (id, ref rank)

let func : t -> t -> t = fun arg res ->
  DS.singleton @@ Function (arg, res)

let get_quants tp =
  let contains id = List.exists (fun id' -> id = id') in
  let rec get_quants acc tp = match DS.find tp with
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

let rec unify tp1 tp2 =

  let rec occurs id tp = match DS.find tp with
    | Variable (id', _) -> id = id'
    | Function (arg, res) -> occurs id arg || occurs id res
  in

  (* TODO: Try not to use [Disjoint_set.update] here. *)
  let rec update_ranks rank tp = match DS.find tp with
    | Variable (id, rank') ->
      DS.update tp @@ Variable (id, min rank rank')
    | Function (arg, res) ->
      update_ranks rank arg;
      update_ranks rank res
  in

  let tp1_desc = DS.find tp1 in
  let tp2_desc = DS.find tp2 in
  match tp1_desc, tp2_desc with
    | Variable (_, rank), _ when !rank = poly_rank ->
      raise_unify tp1 tp2
    | _, Variable (_, rank) when !rank = poly_rank ->
      raise_unify tp1 tp2
    | Variable (id1, _), Variable (id2, _) when id1 = id2->
      assert (tp1_desc == tp2_desc)
    | Variable (id, rank), _ ->
      if occurs id tp2 then raise_occurs id tp2;
      update_ranks rank tp2;
      DS.merge tp2 tp1
    | _, Variable (id, rank) ->
      if occurs id tp1 then raise_occurs id tp1;
      update_ranks rank tp1;
      DS.merge tp1 tp2
    | Function (arg1, res1), Function (arg2, res2) ->
      unify arg1 arg2;
      unify res1 res2;
      DS.merge tp1 tp2

let rec gen rank tp =
  let gen = gen rank in
  match DS.find tp with
    | Variable (id, rank') ->
      if !rank' > rank then
        var poly_rank id
      else
        tp
    | Function (arg, res) ->
      func (gen arg) (gen res)

let inst rank tp =

  let quants = get_quants tp in
  let vars = List.map (fun _ -> var rank @@ Id.fresh_upper ()) quants in
  let env = Id.Map.of_list @@ List.combine quants vars in

  let rec inst tp = match DS.find tp with
    | Variable (id, _) -> Id.Map.find_default tp id env
    | Function (arg, res) -> func (inst arg) (inst res)
  in

  vars, inst tp

(* Utilities *) 

let to_intl_repr tp =

  let module IR = Universal_types.Type in
  let rec to_ir tp = match DS.find tp with
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

  let rec simplify tp = match DS.find tp with
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
    match DS.find tp with
      | Variable (id, _) ->
          Id.to_string id
      | Function (arg, res) ->
        let arg_to_string tp = match DS.find tp with
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

(* External constructors *)

let var = var

let func = func

let func' args res =
  List.fold_left (fun res arg -> func arg res) res (List.rev args)

let get_quants = get_quants

(* Setters *)

let set_rank rank tp = match DS.find tp with
  | Variable (_, rank') -> rank' := rank
  | _ -> error "Type.set_rank: expected variable"
