module Id = Identifier
module Loc = Location

type desc =
  | Variable of Id.t
  | Abstraction of Id.t * Type.t * t
  | Application of t * t
  | Pack of Type.t * t * Type.t
  | Unpack of Id.t * Id.t * t * t

and t = {
  desc : desc ;
  loc : Loc.t ;
}

(* Internal utilities *)

let error : Loc.t -> string -> string -> 'a = fun loc fn_name msg ->
  failwith @@
    Printf.sprintf "%s %s.%s: %s"
      (Loc.to_string loc)
      __MODULE__
      fn_name
      msg

let var : Loc.t -> Id.t -> t = fun loc id ->
  { desc = Variable id; loc }

let abs : Loc.t -> Id.t -> Type.t -> t -> t = fun loc arg tp body ->
  { desc = Abstraction (arg, tp, body); loc }

let app : Loc.t -> t -> t -> t = fun loc fn arg ->
  { desc = Application (fn, arg); loc }

let pack : Loc.t -> Type.t -> t -> Type.t -> t = fun loc tp1 tm tp2 ->
  { desc = Pack (tp1, tm, tp2); loc }

let unpack : Loc.t -> Id.t -> Id.t -> t -> t -> t =
  fun loc tp_id tm_id pack body ->
    { desc = Unpack (tp_id, tm_id, pack, body); loc }

(* Typing *)

let to_type ?(env = Id.Map.empty) =
  let rec to_type tp_bvs env tm = match tm.desc with
    | Variable id ->
      begin try Id.Map.find id env with
        | Id.Unbound id ->
          error tm.loc "to_type" @@
            Printf.sprintf "undefined identifier '%s'" (Id.to_string id)
      end
    | Abstraction (arg, arg_tp, body) ->
      let body_tp =
        to_type tp_bvs (Id.Map.add arg arg_tp env) body
      in
      Type.func arg_tp body_tp
    | Application (fn, arg) ->
      let fn' = to_type tp_bvs env fn in
      let fml_arg_tp, res_tp =
        try
          Type.get_func (Type.beta_reduce ~deep:() ~env fn')
        with Invalid_argument _ ->
          error tm.loc "to_type" @@
            Printf.sprintf
              "expected function type; found '%s'"
              (Type.to_string fn')
      in
      let act_arg_tp = to_type tp_bvs env arg in
      if Type.alpha_equivalent ~beta_env:env act_arg_tp fml_arg_tp then
        res_tp
      else
        error arg.loc "to_type" @@
            Printf.sprintf
              "expected type '%s'; found type '%s'"
              (Type.to_string fml_arg_tp)
              (Type.to_string act_arg_tp)
    | Pack (tp1, tm, tp2) ->
      let tp2_tv, tp2_tp =
        try
          Type.get_exists @@ Type.beta_reduce ~deep:() ~env tp2
        with Invalid_argument _ ->
          error tm.loc "to_type" @@
            Printf.sprintf
              "expected existential type; found '%s'"
              (Type.to_string tp2)
      in
      let tm_tp = to_type tp_bvs env tm in
      let tp2_tp' =
        Type.subst tp_bvs (Id.Map.singleton tp2_tv tp1) tp2_tp
      in
      if Type.alpha_equivalent ~beta_env:env tm_tp tp2_tp' then
        tp2
      else
        error tm.loc "to_type" @@
            Printf.sprintf
              "expected type '%s'; found type '%s'"
              (Type.to_string tp2_tp')
              (Type.to_string tm_tp)
    | Unpack (tp_id, tm_id, pack, body) ->
      let pack_tp = to_type tp_bvs env pack in
      let pack_tp_tv, pack_tp_tp =
        try
          Type.get_exists @@ Type.beta_reduce ~deep:() ~env pack_tp
        with Invalid_argument _ ->
          error tm.loc "to_type" @@
            Printf.sprintf
              "expected existential type; found '%s'"
              (Type.to_string pack_tp)
      in
      let pack_tp_tp' =
        Type.subst
          tp_bvs
          (Id.Map.singleton pack_tp_tv (Type.var @@ Id.to_string tp_id))
          pack_tp_tp
      in
      let res_tp =
        to_type
          (Id.Set.add tp_id tp_bvs)
          (Id.Map.add tm_id pack_tp_tp' env)
          body
      in
      if Id.Set.mem tp_id @@ Type.free_vars res_tp then
        error body.loc "to_type" @@
          Printf.sprintf
            "type '%s' would escape it's scope"
            (Id.to_string tp_id);
      res_tp
  in
  to_type Id.Set.empty env

(* Transformations *)

(** [free_vars tm] computes the free term variables in [tm]. *)
let free_vars : t -> Id.Set.t =
  let rec free_vars fvs tm = match tm.desc with
    | Variable id ->
      Id.Set.add id fvs
    | Abstraction (arg, _, body) ->
      Id.Set.del arg @@ free_vars fvs body
    | Application (fn, arg) ->
      free_vars (free_vars fvs fn) arg
    | Pack (_, tm, _) ->
      free_vars fvs tm
    | Unpack (_, tm_id, pack, body) ->
      Id.Set.del tm_id @@ free_vars (free_vars fvs pack) body
  in
  free_vars Id.Set.empty

(**
  [subst_tp tm id tp'] replaces occurences of [id] in [tm] with [tp'].

  [subst_tp] avoids name capture by renaming binders in [tp] to follow
  the Barendregt convention--i.e. the names of bound variable are chosen
  distinct from those of free variables.
 *)
let subst_tp : t -> Id.t -> Type.t -> t = fun tm id tp' ->
  let rec subst fvs sub tm =
    let loc = tm.loc in
    match tm.desc with
      | Variable _ ->
        tm
      | Abstraction (arg, tp, body) ->
        abs loc arg (Type.subst fvs sub tp) (subst fvs sub body)
      | Application (fn, arg) ->
        app loc (subst fvs sub fn) (subst fvs sub arg)
      | Pack (tp1, tm, tp2) ->
        pack loc
          (Type.subst fvs sub tp1)
          (subst fvs sub tm)
          (Type.subst fvs sub tp2)
      | Unpack (tp_id, tm_id, pack, body) when Id.Set.mem tp_id fvs ->
        let tp_id' = Id.fresh_lower () in
        let fvs' = Id.Set.add tp_id' fvs in
        let sub' =
          Id.Map.add tp_id (Type.var @@ Id.to_string tp_id') sub
        in
        unpack loc tp_id tm_id
          (subst fvs' sub' pack)
          (subst fvs' sub' body)
      | Unpack (tp_id, tm_id, pack, body) ->
        let fvs' = Id.Set.add tp_id fvs in
        let sub' = Id.Map.del tp_id sub in
        unpack loc tp_id tm_id
          (subst fvs' sub' pack)
          (subst fvs' sub' body)
  in
  subst (Type.free_vars tp') (Id.Map.singleton id tp') tm

(**
  [subst_tm tm id tm'] replaces occurences of [id] in [tm] with [tm'].

  As with [subst_tp], [subst_tm] avoids name capture by following the
  Barendregt convention.
 *)
let subst_tm : t -> Id.t -> t -> t = fun tm id tm' ->
  let rec subst fvs sub tm =
    let loc = tm.loc in
    match tm.desc with
      | Variable id ->
        Id.Map.find_default tm id sub
      | Abstraction (arg, tp, body) when Id.Set.mem arg fvs ->
        let arg' = Id.fresh_lower () in
        let sub' = Id.Map.add arg (var Loc.dummy arg') sub in
        abs loc arg' tp @@ subst (Id.Set.add arg' fvs) sub' body
      | Abstraction (arg, tp, body) ->
        abs loc arg tp @@
          subst (Id.Set.add arg fvs) (Id.Map.del arg sub) body
      | Application (fn, arg) ->
        app loc (subst fvs sub fn) (subst fvs sub arg)
      | Pack (tp1, tm, tp2) ->
        pack loc tp1 (subst fvs sub tm) tp2
      | Unpack (tp_id, tm_id, pack, body) when Id.Set.mem tm_id fvs ->
        let tm_id' = Id.fresh_lower () in
        let fvs' = Id.Set.add tm_id' fvs in
        let sub' = Id.Map.add tp_id (var Loc.dummy tm_id') sub in
        unpack loc tp_id tm_id
          (subst fvs' sub' pack)
          (subst fvs' sub' body)
      | Unpack (tp_id, tm_id, pack, body) ->
        let fvs' = Id.Set.add tp_id fvs in
        let sub' = Id.Map.del tp_id sub in
        unpack loc tp_id tm_id
          (subst fvs' sub' pack)
          (subst fvs' sub' body)
  in
  subst (free_vars tm') (Id.Map.singleton id tm') tm

let rec beta_reduce ?deep ?(env = Id.Map.empty) tm =
  let beta_reduce env = beta_reduce ?deep ~env in
  let loc = tm.loc in
  match tm.desc with
    | Variable id ->
      Id.Map.find_default tm id env
    | Abstraction (arg, tp, body) ->
      if deep <> None then
        let env' = Id.Map.del arg env in
        abs loc arg tp @@ beta_reduce env' body
      else
        tm
    | Application (fn, act_arg) ->
      let fn' = beta_reduce env fn in
      let act_arg' = beta_reduce env act_arg in
      begin match fn'.desc with
        | Abstraction (fml_arg, _, body) ->
          let body' = subst_tm body fml_arg act_arg' in
          let env' = Id.Map.del fml_arg env in
          beta_reduce env' body'
        | _ ->
          app loc fn' act_arg'
      end
    | Pack (tp1, tm, tp2) ->
      pack loc tp1 (beta_reduce env tm) tp2
    | Unpack (tp_id, tm_id, pack, body) ->
      let pack' = beta_reduce env pack in
      let body' = beta_reduce env body in
      begin match pack'.desc with
        | Pack (tp, tm, _) ->
          let env' = Id.Map.del tp_id (Id.Map.del tm_id env) in
          beta_reduce env' @@ subst_tp (subst_tm body' tm_id tm) tp_id tp
        | _ ->
          unpack loc tp_id tm_id pack' body'
      end

(* Utilities *)

let alpha_equivalent =
  let rec alpha_equiv tp_env tm_env tm1 tm2 =
    match tm1.desc, tm2.desc with
      | Variable id1, Variable id2 ->
        Id.alpha_equivalent tm_env id1 id2
      | Abstraction (arg1, tp1, body1),
          Abstraction (arg2, tp2, body2) ->
        Type.alpha_equivalent ~env:tp_env tp1 tp2 &&
          alpha_equiv tp_env ((arg1, arg2) :: tm_env) body1 body2
      | Application (fn1, arg1), Application (fn2, arg2) ->
        alpha_equiv tp_env tm_env fn1 fn2 &&
          alpha_equiv tp_env tm_env arg1 arg2
      | Pack (tp11, tm1, tp12), Pack (tp21, tm2, tp22) ->
        Type.alpha_equivalent ~env:tp_env tp11 tp21 &&
          alpha_equiv tp_env tm_env tm1 tm2 &&
          Type.alpha_equivalent ~env:tp_env tp12 tp22
      | Unpack (tp_id_1, tm_id_1, pack1, body1),
          Unpack (tp_id_2, tm_id_2, pack2, body2) ->
        let tp_env' = (tp_id_1, tp_id_2) :: tp_env in
        let tm_env' = (tm_id_1, tm_id_2) :: tm_env in
        alpha_equiv tp_env' tm_env' pack1 pack2 &&
          alpha_equiv tp_env tm_env body1 body2
      | _ ->
        false
  in
  alpha_equiv [] []

let rec to_string tm =
  let to_paren_string tm = Printf.sprintf "(%s)" (to_string tm) in
  match tm.desc with
    | Variable id ->
      Id.to_string id
    | Abstraction (arg, tp, body) ->
      Printf.sprintf "\\%s : %s . %s"
        (Id.to_string arg)
        (Type.to_string tp)
        (to_string body)
    | Application (fn, arg) ->
      let fn_to_string tm = match tm.desc with
        | Variable _ | Application _ -> to_string tm
        | Abstraction _ | Pack _ | Unpack _ -> to_paren_string tm
      in
      let arg_to_string tm = match tm.desc with
        | Variable _ ->
          to_string tm
        | Abstraction _ | Application _ | Pack _ | Unpack _ ->
          to_paren_string tm
      in
      Printf.sprintf "%s %s" (fn_to_string fn) (arg_to_string arg)
    | Pack (tp1, tm, tp2) ->
      Printf.sprintf "pack %s, %s : %s"
        (Type.to_string tp1)
        (to_string tm)
        (Type.to_string tp2)
    | Unpack (tp_id, tm_id, pack, body) ->
      Printf.sprintf "unpack %s, %s = %s in %s"
        (Id.to_string tp_id)
        (Id.to_string tm_id)
        (to_string pack)
        (to_string body)

(* Constructors *)

let var ?(loc = Loc.dummy) id = var loc (Id.of_string id)

let abs ?(loc = Loc.dummy) arg tp body =
  abs loc (Id.of_string arg) tp body

let abs' ?(loc = Loc.dummy) args body =
  let abs' body (arg, tp) = abs ~loc arg tp body in
  List.fold_left abs' body (List.rev args)

let app ?(loc = Loc.dummy) fn arg = app loc fn arg

let app' ?(loc = Loc.dummy) fn args =
  List.fold_left (fun fn args -> app ~loc fn args) fn args

let pack ?(loc = Loc.dummy) tp1 tm tp2 =
  pack loc tp1 tm tp2

let unpack ?(loc = Loc.dummy) tp_id tm_id pack body =
  unpack loc (Id.of_string tp_id) (Id.of_string tm_id) pack body
