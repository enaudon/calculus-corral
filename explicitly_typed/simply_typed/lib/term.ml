module Id = Identifier
module Loc = Location
module Opt = Option
module Type_env = Type.Environment

type desc =
  | Variable of Id.t
  | Abstraction of Id.t * Type.t * t
  | Application of t * t

and t = {
  desc : desc;
  loc : Loc.t;
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
  {desc = Variable id; loc}

let abs : Loc.t -> Id.t -> Type.t -> t -> t = fun loc arg tp body ->
  {desc = Abstraction (arg, tp, body); loc}

let app : Loc.t -> t -> t -> t = fun loc fn arg ->
  {desc = Application (fn, arg); loc}

(* Typing *)

let rec to_type env tm = match tm.desc with
  | Variable id ->
    begin try Type_env.Term.find id env with
      | Id.Unbound id ->
        error tm.loc "to_type" @@
          Printf.sprintf "undefined identifier '%s'" (Id.to_string id)
    end
  | Abstraction (arg, arg_tp, body) ->
    let body_tp = to_type (Type_env.Term.add arg arg_tp env) body in
    Type.func arg_tp body_tp
  | Application (fn, arg) ->
    let fn_tp = to_type env fn in
    let fml_arg_tp, res_tp =
      try
        Type.get_func (Type.beta_reduce ~deep:() env fn_tp)
      with Invalid_argument _ ->
        error tm.loc "to_type" @@
          Printf.sprintf
            "expected function type; found '%s'"
            (Type.to_string fn_tp)
    in
    let act_arg_tp = to_type env arg in
    if Type.alpha_equivalent ~beta_env:env act_arg_tp fml_arg_tp then
      res_tp
    else
      error arg.loc "to_type" @@
        Printf.sprintf
          "expected type '%s'; found type '%s'"
          (Type.to_string fml_arg_tp)
          (Type.to_string act_arg_tp)

(* Transformations *)

(*
  [subst] avoids name capture by renaming binders in [tm] to follow the
  Barendregt convention--i.e. the names of bound variable are chosen
  distinct from those of free variables.
 *)
let rec subst : Id.Set.t -> Env.t -> t -> t = fun fvs sub tm ->
  let loc = tm.loc in
  match tm.desc with
    | Variable id ->
      Env.find_default tm id sub
    | Abstraction (arg, tp, body) when Id.Set.mem arg fvs ->
      let arg' = Id.gen_lower () in
      let sub' = Env.add arg (var Loc.dummy arg') sub in
      abs loc arg' tp @@ subst (Id.Set.add arg' fvs) sub' body
    | Abstraction (arg, tp, body) ->
      abs loc arg tp @@
        subst (Id.Set.add arg fvs) (Env.del arg sub) body
    | Application (fn, arg) ->
      app loc (subst fvs sub fn) (subst fvs sub arg)

let rec beta_reduce ?deep env tm =

  let beta_reduce = beta_reduce ?deep in

  let subst env tm id tm' =
    subst (Id.Set.of_list @@ Env.keys env) (Env.singleton id tm') tm
  in

  let loc = tm.loc in
  match tm.desc with
    | Variable id ->
      Env.find_default tm id env
    | Abstraction (arg, tp, body) ->
      if deep <> Opt.none then
        let env' = Env.add arg (var Loc.dummy arg) env in
        abs loc arg tp @@ beta_reduce env' body
      else
        tm
    | Application (fn, act_arg) ->
      let fn' = beta_reduce env fn in
      let act_arg' = beta_reduce env act_arg in
      match fn'.desc with
        | Abstraction (fml_arg, _, body) ->
          let env' = Env.del fml_arg env in
          beta_reduce env' @@ subst env body fml_arg act_arg'
        | _ ->
          app loc fn' act_arg'

(* Utilities *)

let alpha_equivalent =
  let rec alpha_equiv env tm1 tm2 = match tm1.desc, tm2.desc with
    | Variable id1, Variable id2 ->
      Id.alpha_equivalent env id1 id2
    | Abstraction (arg1, tp1, body1), Abstraction (arg2, tp2, body2) ->
      Type.alpha_equivalent tp1 tp2 &&
        alpha_equiv ((arg1, arg2) :: env) body1 body2
    | Application (fn1, arg1), Application (fn2, arg2) ->
      alpha_equiv env fn1 fn2 && alpha_equiv env arg1 arg2
    | _ ->
      false
  in
  alpha_equiv []

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
        | Abstraction _ -> to_paren_string tm
      in
      let arg_to_string tm = match tm.desc with
        | Variable _ -> to_string tm
        | Abstraction _ | Application _ -> to_paren_string tm
      in
      Printf.sprintf "%s %s" (fn_to_string fn) (arg_to_string arg)

(* Containers *)

module Environment = Env

(* Constructors *)

let var ?(loc = Loc.dummy) id = var loc id

let abs ?(loc = Loc.dummy) arg tp body = abs loc arg tp body

let abs' ?(loc = Loc.dummy) args body =
  List.fold_right (fun (arg, tp) body -> abs ~loc arg tp body) args body

let app ?(loc = Loc.dummy) fn arg = app loc fn arg

let app' ?(loc = Loc.dummy) fn args = List.fold_left (app ~loc) fn args
