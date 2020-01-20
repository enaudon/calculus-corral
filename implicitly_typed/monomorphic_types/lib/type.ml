module Id = Identifier
module Misc = Miscellaneous
module Opt = Option

type t =
  | Inference_variable of Id.t
  | Function of t * t

module Environment = Type_environment.Make (struct
  type value = t

  let initial_types = []

  let initial_terms = []
end)

(* Exceptions *)

exception Occurs of Id.t * t

(* Constructors *)

let inf_var id = Inference_variable id

let func arg res = Function (arg, res)

let func' args res = List.fold_right func args res

(* Inference *)

module Inferencer : sig
  type state

  val initial : state

  val unify : state -> t -> t -> state

  val apply : state -> t -> t
end = struct
  module Sub : sig
    type s = t Id.Map.t

    val identity : s

    val extend : Id.t -> t -> s -> s

    val apply : t -> s -> t
  end = struct
    type s = t Id.Map.t

    let identity = Id.Map.empty

    let singleton : Id.t -> t -> s = Id.Map.singleton

    let rec apply : t -> s -> t =
     fun tp sub ->
       match tp with
         | Inference_variable id ->
           Id.Map.find_default tp id sub
         | Function (arg, res) ->
           func (apply arg sub) (apply res sub)

    let extend id tp sub =
      let apply tp' = apply tp' @@ singleton id tp in
      Id.Map.add id tp @@ Id.Map.map apply sub

    let apply tp sub =
      let tp' = apply tp sub in
      assert (tp' = apply tp' sub);
      tp'
  end

  type state = Sub.s

  let raise_occurs : Id.t -> t -> 'a = fun id tp -> raise @@ Occurs (id, tp)

  let initial = Sub.identity

  let unify state tp1 tp2 =
    let rec occurs : Id.t -> t -> bool =
     fun id tp ->
       match tp with
         | Inference_variable id' ->
           id = id'
         | Function (arg, res) ->
           occurs id arg || occurs id res
    in
    let rec unify state tp1 tp2 =
      let tp1' = Sub.apply tp1 state in
      let tp2' = Sub.apply tp2 state in
      match (tp1', tp2') with
        | Inference_variable id1, Inference_variable id2 when id1 = id2 ->
          state
        | _, Inference_variable id ->
          if occurs id tp1' then raise_occurs id tp1';
          Sub.extend id tp1' state
        | Inference_variable id, _ ->
          if occurs id tp2' then raise_occurs id tp2';
          Sub.extend id tp2' state
        | Function (arg1, res1), Function (arg2, res2) ->
          unify (unify state arg1 arg2) res1 res2
    in
    let state' = unify state tp1 tp2 in
    assert (Sub.apply tp1 state' = Sub.apply tp2 state');
    state'

  let apply state tp = Sub.apply tp state
end

(* Utilities *)

let simplify tp =
  let fresh =
    let cntr = ref (-1) in
    fun () ->
      incr cntr;
      Id.define @@ Misc.int_to_upper !cntr
  in
  let simplify_id =
    let env = Hashtbl.create 1024 in
    fun id ->
      try Hashtbl.find env id
      with Not_found ->
        let id' = fresh () in
        Hashtbl.add env id id';
        id'
  in
  let rec simplify tp =
    match tp with
      | Inference_variable id ->
        inf_var @@ simplify_id id
      | Function (arg, res) ->
        let arg' = simplify arg in
        let res' = simplify res in
        func arg' res'
  in
  simplify tp

let to_string ?no_simp tp =
  let rec to_string tp =
    let to_paren_string tp = Printf.sprintf "(%s)" (to_string tp) in
    match tp with
      | Inference_variable id ->
        Printf.sprintf "'%s" (Id.to_string id)
      | Function (arg, res) ->
        let arg_to_string tp =
          match tp with
            | Inference_variable _ ->
              to_string tp
            | Function _ ->
              to_paren_string tp
        in
        Printf.sprintf "%s -> %s" (arg_to_string arg) (to_string res)
  in
  to_string @@ if no_simp = Opt.none then simplify tp else tp
