module Id = Identifier
module DS = Disjoint_set

type t = desc DS.t

and desc =
  | Variable of Id.t
  | Function of t * t

(* Exceptions *)

exception Occurs of Id.t * t

let raise_occurs : Id.t -> t -> 'a = fun id tp ->
  raise @@ Occurs (id, tp)

(* Internal constructors *)

let var : Id.t -> t = fun id -> DS.singleton @@ Variable id

let func : t -> t -> t = fun arg res ->
  DS.singleton @@ Function (arg, res)

(* Inference *)

let rec unify tp1 tp2 =

  let rec occurs id tp = match DS.find tp with
    | Variable id' -> id = id'
    | Function (arg, res) -> occurs id arg || occurs id res
  in

  let tp1_desc = DS.find tp1 in
  let tp2_desc = DS.find tp2 in
  match tp1_desc, tp2_desc with
    | Variable id1, Variable id2 when id1 = id2->
      assert (tp1_desc == tp2_desc)
    | Variable id, _ ->
      if occurs id tp2 then raise_occurs id tp2;
      DS.merge tp2 tp1
    | _, Variable id ->
      if occurs id tp1 then raise_occurs id tp1;
      DS.merge tp1 tp2
    | Function (arg1, res1), Function (arg2, res2) ->
      unify arg1 arg2;
      unify res1 res2;
      DS.merge tp1 tp2

(* Utilities *) 

let simplify tp =

  let fresh =
    let cntr = ref (-1) in
    fun () ->
      incr cntr;
      Id.of_string @@ Printf.sprintf "_%d" !cntr
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
    | Variable id ->
      var @@ simplify_id id
    | Function (arg, res) ->
      let arg' = simplify arg in
      let res' = simplify res in
      func arg' res'
  in

  simplify tp

let to_string ?no_simp tp =
  let rec to_string tp =
    let to_paren_string tp = Printf.sprintf "(%s)" (to_string tp) in
    match DS.find tp with
      | Variable id ->
        Id.to_string id
      | Function (arg, res) ->
        let arg_to_string tp = match DS.find tp with
          | Variable _ -> to_string tp
          | Function _ -> to_paren_string tp
        in
        Printf.sprintf "%s -> %s" (arg_to_string arg) (to_string res)
  in
  to_string @@ if no_simp = None then simplify tp else tp

(* External constructors *)

let var = var

let func = func

let func' args res =
  List.fold_left (fun res arg -> func arg res) res (List.rev args)
