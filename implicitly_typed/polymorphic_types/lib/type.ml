module Id = Identifier
module DS = Disjoint_set
module Misc = Miscellaneous

type mono = desc DS.t

and desc =
  | Variable of Id.t * int
  | Function of mono * mono

type t = { quants : Id.t list; body : mono }

(* Exceptions *)

exception Occurs of Id.t * t

exception Cannot_unify of t * t

exception Expected_mono

let raise_occurs : Id.t -> t -> 'a = fun id tp ->
  raise @@ Occurs (id, tp)

let raise_unify : t -> t -> 'a = fun tp1 tp2 ->
  raise @@ Cannot_unify (tp1, tp2)

let raise_exp_mono : unit -> 'a = fun () -> raise @@ Expected_mono

(* Internal constructors *)

let var : int -> Id.t -> mono = fun rank id ->
  DS.singleton @@ Variable (id, rank)

let func : mono -> mono -> mono = fun arg res ->
  DS.singleton @@ Function (arg, res)

let scheme : Id.t list -> mono -> t = fun quants body ->
  { quants; body }

(* Inference *)

let unify tp1 tp2 =

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

  let rec unify m1 m2 =
    let m1_desc = DS.find m1 in
    let m2_desc = DS.find m2 in
    match m1_desc, m2_desc with
      | Variable (id1, _), Variable (id2, _) when id1 = id2->
        assert (m1_desc == m2_desc)
      | Variable (id, rank), _ ->
        if occurs id m2 then raise_occurs id @@ scheme tp2.quants m2;
        update_ranks rank m2;
        DS.merge m2 m1
      | _, Variable (id, rank) ->
        if occurs id m1 then raise_occurs id @@ scheme tp1.quants m1;
        update_ranks rank m2;
        DS.merge m1 m2
      | Function (arg1, res1), Function (arg2, res2) ->
        unify arg1 arg2;
        unify res1 res2;
        DS.merge m1 m2
  in
  
  match tp1.quants, tp2.quants with
    | [], [] -> unify tp1.body tp2.body
    | _, _ -> raise_unify tp1 tp2

let gen rank tp =

  let contains id = List.exists (fun id' -> id = id') in

  let rec get_scheme_vars acc tp = match DS.find tp with
    | Variable (id, rank') ->
      if rank' > rank && not @@ contains id acc then
        id :: acc
      else
        acc
    | Function (arg, res) ->
      get_scheme_vars (get_scheme_vars acc arg) res
  in

  if tp.quants <> [] then raise_exp_mono ();
  { tp with quants = List.rev @@ get_scheme_vars [] tp.body }

let inst rank { quants; body } =

  let env =
    Id.Map.of_list @@
      List.map (fun id -> id, var rank @@ Id.fresh_upper ()) quants
  in

  let rec inst tp = match DS.find tp with
    | Variable (id, _) -> Id.Map.find_default tp id env
    | Function (arg, res) -> func (inst arg) (inst res)
  in

  { quants = []; body = inst body }

(* Utilities *) 

let simplify { quants; body } =

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
      var rank @@ simplify_id id
    | Function (arg, res) ->
      let arg' = simplify arg in
      let res' = simplify res in
      func arg' res'
  in

  let quants = List.map simplify_id quants in
  let body = simplify body in
  { quants; body }

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

  let { quants; body } = if no_simp = None then simplify tp else tp in
  if quants = [] || show_quants = None then
    to_string body
  else
    Printf.sprintf "forall %s . %s"
      (String.concat " " @@ List.map Id.to_string quants)
      (to_string body)

(* External constructors *)

let schemify = scheme []

let var rank id = schemify @@ var rank id

let func arg res = match arg.quants, res.quants with
  | [], [] -> schemify @@ func arg.body res.body
  | _, _ -> raise_exp_mono ()

let func' args res =
  List.fold_left (fun res arg -> func arg res) res (List.rev args)
