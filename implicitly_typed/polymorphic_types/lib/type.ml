module Id = Identifier
module Misc = Miscellaneous

type mono =
  | Variable of Id.t
  | Function of mono * mono

type t = {
  quants : Id.t list ;
  body : mono ;
}

(* Exceptions *)

exception Occurs of Id.t * t

exception Cannot_unify of t * t

exception Expected_mono

(* Internal functions *)

let error : string -> 'a = fun msg -> failwith msg

let raise_occurs : Id.t -> t -> 'a = fun id tp ->
  raise @@ Occurs (id, tp)

let raise_unify : t -> t -> 'a = fun tp1 tp2 ->
  raise @@ Cannot_unify (tp1, tp2)

let raise_exp_mono : unit -> 'a = fun () -> raise @@ Expected_mono

let var : Id.t -> mono = fun id -> Variable id

let func : mono -> mono -> mono = fun arg res -> Function (arg, res)

let scheme : Id.t list -> mono -> t = fun quants body ->
  { quants; body }

(* Inference *)

module Substitution : sig

  type s

  val identity : s

  val extend_mono : Id.t -> mono -> s -> s

  val apply : t -> s -> t

  val apply_mono : mono -> s -> mono

end = struct

  type nonrec s = mono Id.Map.t

  let identity = Id.Map.empty

  let singleton : Id.t -> mono -> s = Id.Map.singleton

  let rec apply_mono m sub = match m with
    | Variable id ->
      Id.Map.find_default m id sub
    | Function (arg, res) ->
      func (apply_mono arg sub) (apply_mono res sub)

  let extend_mono id m sub =
    Id.Map.add id m @@
      Id.Map.map (fun m' -> apply_mono m' @@ singleton id m) sub

  let apply tp sub =
    let body = apply_mono tp.body sub in
    assert (body = apply_mono body sub);
    scheme tp.quants body

end

module Rank : sig

  val inc : unit -> unit
  val dec : unit -> Id.Set.t

  val register : Id.t -> unit
  val unregister : Id.t -> unit

  val is_mono : Id.t -> bool

  val update : Id.t -> Id.t -> unit

end = struct

  let init = 0

  let void = -1

  let curr, inc, dec, add, del =
    let pools : (int, Id.Set.t) Hashtbl.t = Hashtbl.create 32 in
    let rank : int ref = ref void in
    let curr () = !rank in
    let inc () =
      incr rank;
      Hashtbl.add pools !rank Id.Set.empty
    in
    let dec () =
      let pool = Hashtbl.find pools !rank in
      Hashtbl.remove pools !rank;
      decr rank;
      pool
    in
    let add r id =
      try
        Hashtbl.replace pools r @@ Id.Set.add id @@ Hashtbl.find pools r
      with Not_found ->
        failwith @@ Printf.sprintf
          "Type.Rank.add: Not_found %s.%d.  Rank is %d"
          (Id.to_string id)
          r
          !rank
    in
    let del r id =
      try
        Hashtbl.replace pools r @@ Id.Set.del id @@ Hashtbl.find pools r
      with Not_found ->
        failwith @@ Printf.sprintf
          "Type.Rank.add: Not_found %s.%d.  Rank is %d"
          (Id.to_string id)
          r
          !rank
    in
    curr, inc, dec, add, del

  let get, set =
    let ranks : (Id.t, int) Hashtbl.t = Hashtbl.create 2048 in
    Hashtbl.find ranks, Hashtbl.replace ranks

  let is_mono id = get id >= init

  let register id =
    let r = curr () in
    add r id;
    set id r

  let unregister id =
    del (get id) id

  let update id1 id2 =
    let r = get id1 in
    let r' = min r (get id2) in
    del r id1;
    add r' id1;
    set id1 r'

end

module Sub = Substitution

let unify sub tp1 tp2 =

  let rec occurs : Id.t -> mono -> bool = fun id tp -> match tp with
    | Variable id' -> id = id'
    | Function (arg, res) -> occurs id arg || occurs id res
  in

  let rec update_ranks : Id.t -> mono -> unit = fun id tp ->
    match tp with
      | Variable id' ->
        Rank.update id' id
      | Function (arg, res) ->
        update_ranks id arg;
        update_ranks id res
  in

  let merge : Id.t -> mono -> Sub.s -> Sub.s = fun id m sub ->
    Rank.unregister id;
    Sub.extend_mono id m sub
  in

  let rec unify sub m1 m2 =
    let m1' = Sub.apply_mono m1 sub in
    let m2' = Sub.apply_mono m2 sub in
    match m1', m2' with
      | Variable id, _ when not (Rank.is_mono id) ->
        raise_unify (scheme tp1.quants m1') (scheme tp2.quants m2')
      | _, Variable id when not (Rank.is_mono id) ->
        raise_unify (scheme tp1.quants m1') (scheme tp2.quants m2')
      | Variable id1, Variable id2 when id1 = id2->
        sub
      | Variable id, _ ->
        if occurs id m2' then raise_occurs id (scheme tp2.quants m2');
        update_ranks id m2';
        merge id m2' sub
      | _, Variable id ->
        if occurs id m1' then raise_occurs id (scheme tp2.quants m1');
        update_ranks id m1';
        merge id m1' sub
      | Function (arg1, res1), Function (arg2, res2) ->
        unify (unify sub arg1 arg2) res1 res2
  in

  if tp1.quants <> [] || tp2.quants <> [] then
    raise_unify tp1 tp2;

  let sub' = unify sub tp1.body tp2.body in
  assert (Sub.apply tp1 sub' = Sub.apply tp2 sub');
  sub'

let register m = match m with
  | Variable id -> Rank.register id
  | _ -> error "Type.register: expected variable"

let gen_enter () = Rank.inc ()

(* [free_vars tp] computes the free variables in [tp]. *)
let free_vars tp =
  let rec free_vars (seen, fvs) tp = match tp with
    | Variable id ->
      if Id.Set.mem id seen then
        seen, fvs
      else
        Id.Set.add id seen, id :: fvs
    | Function (arg, res) ->
      free_vars (free_vars (seen, fvs) arg) res
  in
  List.rev @@ snd @@ free_vars (Id.Set.empty, []) tp

let gen_exit sub tp =

  let tp = Sub.apply tp sub in

  if tp.quants <> [] then
    raise_exp_mono ();

  let qvs = Rank.dec () in
  let pred id = Id.Set.mem id qvs in
  let inc, _ = List.partition pred @@ free_vars tp.body in
  let tp' = { quants = inc; body = tp.body } in

  qvs, tp'

let inst sub tp =

  let tp = Sub.apply tp sub in

  let quants = tp.quants in
  let vars = List.map (fun _ -> var @@ Id.fresh_upper ()) quants in
  List.iter register vars;
  let env = Id.Map.of_list @@ List.combine quants vars in

  let rec inst m = match m with
    | Variable id -> Id.Map.find_default m id env
    | Function (arg, res) -> func (inst arg) (inst res)
  in

  List.map (scheme []) vars, scheme [] @@ inst tp.body

(* Utilities *)

let to_intl_repr tp =

  let module IR = Universal_types.Type in
  let rec to_ir tp = match tp with
    | Variable id -> IR.var id
    | Function (arg, res) -> IR.func (to_ir arg) (to_ir res)
  in

  IR.forall' tp.quants @@ to_ir tp.body

(*
  NOTE: [simplify] does not register the new variables that it creates
  with [Rank], so [simplify]'d types cannot be used with inference
  functions.
 *)
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

  let rec simplify tp = match tp with
    | Variable id ->
      var @@ simplify_id id
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

  let { quants; body } = if no_simp = None then simplify tp else tp in
  if quants = [] || show_quants = None then
    to_string body
  else
    Printf.sprintf "forall %s . %s"
      (String.concat " " @@ List.map Id.to_string quants)
      (to_string body)

(* External functions *)

let var id = scheme [] @@ var id

let func arg res = match arg.quants, res.quants with
  | [], [] -> scheme [] @@ func arg.body res.body
  | _, _ -> raise_exp_mono ()

let func' args res =
  List.fold_left (fun res arg -> func arg res) res (List.rev args)

let get_quants { quants; _ } = quants

let register tp = register tp.body
