module Id = Identifier
module Loc = Location

type desc =
  | Variable of Id.t
  | Abstraction of Id.t * Type.t * t
  | Application of t * t

and t = {
  desc : desc ;
  loc : Loc.t ;
}

let error : Loc.t -> string -> 'a = fun loc msg ->
  failwith @@ Printf.sprintf "%s %s" (Loc.to_string loc) msg

(* Typing *)

let to_type =
  let rec to_type env tm = match tm.desc with
    | Variable id ->
      begin try Id.Map.find id env with
        | Id.Unbound id ->
          failwith @@
            Printf.sprintf
              "%s: Undefined identifier '%s'\n%!"
              (Loc.to_string tm.loc)
              (Id.to_string id)
      end
    | Abstraction (arg, arg_tp, body) ->
      let body_tp = to_type (Id.Map.add arg arg_tp env) body in
      Type.func arg_tp body_tp
    | Application (fn, arg) ->
      let fml_arg_tp, res_tp = try Type.get_func @@ to_type env fn with
        | Invalid_argument _ ->
          error tm.loc @@
            Printf.sprintf
              "Term.to_type: cannot apply non-function"
      in
      let act_arg_tp = to_type env arg in
      if Type.equals act_arg_tp fml_arg_tp then
        res_tp
      else
      error arg.loc @@
          Printf.sprintf
            "Term.to_type: expected type '%s'; found type '%s'"
              (Type.to_string fml_arg_tp)
              (Type.to_string act_arg_tp)
  in
  to_type Id.Map.empty

(* Utilities *)

let rec to_string tm =
  let to_paren_string tm = Printf.sprintf "(%s)" (to_string tm) in
  match tm.desc with
    | Variable id ->
      Id.to_string id
    | Abstraction (arg, tp, body) ->
      Printf.sprintf "%s : %s . %s"
        (Id.to_string arg)
        (Type.to_string tp)
        (to_string body)
    | Application (fn, arg) ->
      let fn_to_string tm = match tm.desc with
        | Variable _ -> to_string tm
        | Abstraction _ -> to_paren_string tm
        | Application _ -> to_string tm
      in
      let arg_to_string tm = match tm.desc with
        | Variable _ -> to_string tm
        | Abstraction _ -> to_paren_string tm
        | Application _ -> to_paren_string tm
      in
      Printf.sprintf "%s %s" (fn_to_string fn) (arg_to_string arg)

(* Constructors *)

let var ?(loc = Loc.dummy) id =
  { desc = Variable (Id.of_string id); loc }

let abs ?(loc = Loc.dummy) arg tp body =
  { desc = Abstraction (Id.of_string arg, tp, body); loc }

let abs' ?(loc = Loc.dummy) args body =
  let abs' body (arg, tp) = abs ~loc arg tp body in
  List.fold_left abs' body (List.rev args)

let app ?(loc = Loc.dummy) fn arg =
  { desc = Application (fn, arg); loc }

let app' ?(loc = Loc.dummy) fn args =
  List.fold_left (fun fn args -> app ~loc fn args) fn args
