module Id = Identifier
module Loc = Location
module Misc = Miscellaneous
module Type_env = Type.Environment

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

module Env = Environment.Make (struct
  type value = t
  let initial = []
end)

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

let rec to_type (kn_env, tp_env) tm =
  let to_type kn_env tp_env = to_type (kn_env, tp_env) in
  match tm.desc with
    | Variable id ->
      begin try Type_env.Term.find id tp_env with
        | Id.Unbound id ->
          error tm.loc "to_type" @@
            Printf.sprintf "undefined identifier '%s'" (Id.to_string id)
      end
    | Abstraction (arg, arg_tp, body) ->
      let tp_env' = Type_env.Term.add arg arg_tp tp_env in
      let body_tp = to_type kn_env tp_env' body in
      Type.func arg_tp body_tp
    | Application (fn, arg) ->
      let fn_tp = to_type kn_env tp_env fn in
      let fml_arg_tp, res_tp =
        try
          Type.get_func (Type.beta_reduce ~deep:() tp_env fn_tp)
        with Invalid_argument _ ->
          error tm.loc "to_type" @@
            Printf.sprintf
              "expected function type; found '%s'"
              (Type.to_string fn_tp)
      in
      let act_arg_tp = to_type kn_env tp_env arg in
      let beta_env = tp_env in
      if Type.alpha_equivalent ~beta_env act_arg_tp fml_arg_tp then
        res_tp
      else
        error arg.loc "to_type" @@
            Printf.sprintf
              "expected type '%s'; found type '%s'"
              (Type.to_string fml_arg_tp)
              (Type.to_string act_arg_tp)
    | Pack (tp1, tm, tp2) ->
      Type.check kn_env tp2;
      let tp2_tv, tp2_tp =
        try
          Type.get_exists @@ Type.beta_reduce ~deep:() tp_env tp2
        with Invalid_argument _ ->
          error tm.loc "to_type" @@
            Printf.sprintf
              "expected existential type; found '%s'"
              (Type.to_string tp2)
      in
      let tm_tp = to_type kn_env tp_env tm in
      let tp2_tp' =
        Type.subst
          (Id.Set.of_list @@ Type_env.Type.keys tp_env)
          (Type_env.Type.singleton tp2_tv tp1)
          tp2_tp
      in
      if Type.alpha_equivalent ~beta_env:tp_env tm_tp tp2_tp' then
        tp2
      else
        error tm.loc "to_type" @@
            Printf.sprintf
              "expected type '%s'; found type '%s'"
              (Type.to_string tp2_tp')
              (Type.to_string tm_tp)
    | Unpack (tp_id, tm_id, pack, body) ->
      let pack_tp = to_type kn_env tp_env pack in
      let pack_tp_tv, pack_tp_tp =
        try
          Type.get_exists @@ Type.beta_reduce ~deep:() tp_env pack_tp
        with Invalid_argument _ ->
          error tm.loc "to_type" @@
            Printf.sprintf
              "expected existential type; found '%s'"
              (Type.to_string pack_tp)
      in
      let pack_tp_tp' =
        Type.subst
          (Id.Set.of_list @@ Type_env.Type.keys tp_env)
          (Type_env.Type.singleton pack_tp_tv @@ Type.var tp_id)
          pack_tp_tp
      in
      let tp_env' = Type_env.Term.add tm_id pack_tp_tp' tp_env in
      let res_tp = to_type kn_env tp_env' body in
      if Id.Set.mem tp_id @@ Type.free_vars res_tp then
        error body.loc "to_type" @@
          Printf.sprintf
            "type '%s' would escape it's scope"
            (Id.to_string tp_id);
      res_tp

(* Transformations *)

(*
  [subst_tp fvs sub tp] applies [sub] to [tp], replacing any variable in
  the domain of [sub] with the corresponding type the range of [sub].
  [fvs] is any superset of the variables which appear in the range of
  [sub].

  [subst_tp] avoids name capture by renaming binders in [tp] to follow
  the Barendregt convention--i.e. the names of bound variable are chosen
  distinct from those of free variables.
 *)
let rec subst_tp : Id.Set.t -> Type_env.t -> t -> t = fun fvs sub tm ->
  let loc = tm.loc in
  match tm.desc with
    | Variable _ ->
      tm
    | Abstraction (arg, tp, body) ->
      abs loc arg (Type.subst fvs sub tp) (subst_tp fvs sub body)
    | Application (fn, arg) ->
      app loc (subst_tp fvs sub fn) (subst_tp fvs sub arg)
    | Pack (tp1, tm, tp2) ->
      pack loc
        (Type.subst fvs sub tp1)
        (subst_tp fvs sub tm)
        (Type.subst fvs sub tp2)
    | Unpack (tp_id, tm_id, pack, body) when Id.Set.mem tp_id fvs ->
      let tp_id' = Id.gen_upper () in
      let fvs' = Id.Set.add tp_id' fvs in
      let sub' =
        Type_env.Type.add tp_id (Type.var tp_id') sub
      in
      unpack loc tp_id' tm_id
        (subst_tp fvs' sub' pack)
        (subst_tp fvs' sub' body)
    | Unpack (tp_id, tm_id, pack, body) ->
      let fvs' = Id.Set.add tp_id fvs in
      let sub' = Type_env.Type.del tp_id sub in
      unpack loc tp_id tm_id
        (subst_tp fvs' sub' pack)
        (subst_tp fvs' sub' body)

(**
  As with [subst_tp], [subst_tm] avoids name capture by following the
  Barendregt convention.
 *)
let rec subst_tm : Id.Set.t -> Env.t -> t -> t = fun fvs sub tm ->
  let loc = tm.loc in
  match tm.desc with
    | Variable id ->
      Env.find_default tm id sub
    | Abstraction (arg, tp, body) when Id.Set.mem arg fvs ->
      let arg' = Id.gen_lower () in
      let sub' = Env.add arg (var Loc.dummy arg') sub in
      abs loc arg' tp @@ subst_tm (Id.Set.add arg' fvs) sub' body
    | Abstraction (arg, tp, body) ->
      abs loc arg tp @@
        subst_tm (Id.Set.add arg fvs) (Env.del arg sub) body
    | Application (fn, arg) ->
      app loc (subst_tm fvs sub fn) (subst_tm fvs sub arg)
    | Pack (tp1, tm, tp2) ->
      pack loc tp1 (subst_tm fvs sub tm) tp2
    | Unpack (tp_id, tm_id, pack, body) when Id.Set.mem tm_id fvs ->
      let tm_id' = Id.gen_lower () in
      let fvs' = Id.Set.add tm_id' fvs in
      let sub' = Env.add tp_id (var Loc.dummy tm_id') sub in
      unpack loc tp_id tm_id
        (subst_tm fvs' sub' pack)
        (subst_tm fvs' sub' body)
    | Unpack (tp_id, tm_id, pack, body) ->
      let fvs' = Id.Set.add tp_id fvs in
      let sub' = Env.del tp_id sub in
      unpack loc tp_id tm_id
        (subst_tm fvs' sub' pack)
        (subst_tm fvs' sub' body)

let rec beta_reduce ?deep env tm =

  let beta_reduce = beta_reduce ?deep in

  let subst_tp env tm id tp =
    let fvs = Id.Set.of_list @@ Env.keys env in
    subst_tp fvs (Type_env.Type.singleton id tp) tm
  in

  let subst_tm env tm id tm' =
    subst_tm (Id.Set.of_list @@ Env.keys env) (Env.singleton id tm') tm
  in

  let loc = tm.loc in
  match tm.desc with
    | Variable id ->
      Env.find_default tm id env
    | Abstraction (arg, tp, body) ->
      if deep <> None then
        let env' = Env.add arg (var Loc.dummy arg) env in
        abs loc arg tp @@ beta_reduce env' body
      else
        tm
    | Application (fn, act_arg) ->
      let fn' = beta_reduce env fn in
      let act_arg' = beta_reduce env act_arg in
      begin match fn'.desc with
        | Abstraction (fml_arg, _, body) ->
          let body' = subst_tm env body fml_arg act_arg' in
          beta_reduce env body'
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
          beta_reduce env @@
            subst_tp env (subst_tm env body' tm_id tm) tp_id tp
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

let simplify tm =

  let fresh =
    let cntr = ref (-1) in
    fun () ->
      incr cntr;
      Id.define @@ Misc.int_to_upper !cntr
  in

  let rec simplify env tm =
    let loc = tm.loc in
    match tm.desc with
      | Variable _ ->
        tm
      | Abstraction (arg, tp, body) ->
        let tp' = Type.simplify ~ctx:(fresh, env) tp in
        let body' = simplify env body in
        abs loc arg tp' body'
      | Application (fn, arg) ->
        let fn' = simplify env fn in
        let arg' = simplify env arg in
        app loc fn' arg'
      | Pack (tp1, tm, tp2) ->
        let tp1' = Type.simplify ~ctx:(fresh, env) tp1 in
        let tm' = simplify env tm in
        let tp2' = Type.simplify ~ctx:(fresh, env) tp2 in
        pack loc tp1' tm' tp2'
      | Unpack (tp_id, tm_id, pack, body) when Id.is_generated tp_id ->
        let tp_id' = fresh () in
        let env' = Id.Map.add tp_id (Type.var tp_id') env in
        let pack' = simplify env' pack in
        let body' = simplify env' body in
        unpack loc tp_id' tm_id pack' body'
      | Unpack (tp_id, tm_id, pack, body) ->
        let pack = simplify env pack in
        let body = simplify env body in
        unpack loc tp_id tm_id pack body
  in

  simplify Id.Map.empty tm

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

module Environment = Env

let var ?(loc = Loc.dummy) id = var loc id

let abs ?(loc = Loc.dummy) arg tp body = abs loc arg tp body

let abs' ?(loc = Loc.dummy) args body =
  List.fold_right (fun (arg, tp) body -> abs ~loc arg tp body) args body

let app ?(loc = Loc.dummy) fn arg = app loc fn arg

let app' ?(loc = Loc.dummy) fn args = List.fold_left (app ~loc) fn args

let pack ?(loc = Loc.dummy) tp1 tm tp2 = pack loc tp1 tm tp2

let unpack ?(loc = Loc.dummy) tp_id tm_id pack body =
  unpack loc tp_id tm_id pack body
