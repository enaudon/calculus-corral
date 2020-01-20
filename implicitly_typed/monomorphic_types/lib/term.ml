module Id = Identifier
module Infer = Type.Inferencer
module Loc = Location
module Type_env = Type.Environment

type desc =
  | Variable of Id.t
  | Abstraction of Id.t * t
  | Application of t * t

and t =
  { desc : desc;
    loc : Loc.t }

(* Internal utilities *)

let error : Loc.t -> string -> string -> 'a =
 fun loc fn_name msg ->
   failwith
   @@ Printf.sprintf "%s %s.%s: %s" (Loc.to_string loc) __MODULE__ fn_name msg

let var : Loc.t -> Id.t -> t = fun loc id -> {desc = Variable id; loc}

let abs : Loc.t -> Id.t -> t -> t =
 fun loc arg body -> {desc = Abstraction (arg, body); loc}

let app : Loc.t -> t -> t -> t =
 fun loc fn arg -> {desc = Application (fn, arg); loc}

(* Typing *)

(* [to_type_hm env tm] ensures that [tm] has type [tp], via Algorithm W-style
   Hindley-Milner type inference. [tm] is assumed to be closed under [env]. *)
let to_type_hm : Type_env.t -> t -> Type.t =
 fun env tm ->
   let fresh_inf_var () = Type.inf_var @@ Id.gen_upper () in
   let unify loc state tp1 tp2 =
     try Infer.unify state tp1 tp2
     with Type.Occurs (id, tp) ->
       error loc "to_type_hm"
       @@ Printf.sprintf
            "type variable '%s' occurs in '%s'"
            (Id.to_string id)
            (Type.to_string ~no_simp:() tp)
   in
   let rec to_type env state exp_tp tm =
     let loc = tm.loc in
     match tm.desc with
       | Variable id ->
         let tp =
           try Type_env.Term.find id env
           with Id.Unbound id ->
             error tm.loc "to_type_hm"
             @@ Printf.sprintf "undefined identifier '%s'" (Id.to_string id)
         in
         unify loc state exp_tp tp
       | Abstraction (arg, body) ->
         let arg_tp = fresh_inf_var () in
         let body_tp = fresh_inf_var () in
         let env' = Type_env.Term.add arg arg_tp env in
         let state = to_type env' state body_tp body in
         unify loc state exp_tp @@ Type.func arg_tp body_tp
       | Application (fn, arg) ->
         let tp = fresh_inf_var () in
         let state = to_type env state (Type.func tp exp_tp) fn in
         to_type env state tp arg
   in
   let tp = fresh_inf_var () in
   let state = to_type env Infer.initial tp tm in
   Infer.apply state tp

(* [to_type_pr env tp tm] ensures that [tm] has type [tp], via constraint-based
   type inference a la Pottier and Remy. [tm] is assumed to be closed under
   [env]. *)
let to_type_pr : Type_env.t -> t -> Type.t =
 fun env tm ->
   let module TC = Type_constraint in
   let open TC.Operators in
   let rec constrain exp_tp tm =
     let loc = tm.loc in
     match tm.desc with
       | Variable id ->
         TC.var_eq ~loc id exp_tp
       | Abstraction (arg, body) ->
         TC.exists ~loc (fun arg_tp ->
             TC.exists ~loc @@ fun body_tp ->
             TC.conj
               (TC.def arg arg_tp @@ constrain body_tp body)
               (TC.type_eq exp_tp @@ Type.func arg_tp body_tp))
         <$> fun _ -> ()
       | Application (fn, arg) ->
         TC.exists ~loc (fun arg_tp ->
             TC.conj
               (constrain (Type.func arg_tp exp_tp) fn)
               (constrain arg_tp arg))
         <$> fun _ -> ()
   in
   TC.solve
     env
     (TC.exists ~loc:tm.loc (fun tp -> constrain tp tm) <$> fun (tp, _) -> tp)

(* Utilities *)

let rec to_string tm =
  let to_paren_string tm = Printf.sprintf "(%s)" (to_string tm) in
  match tm.desc with
    | Variable id ->
      Id.to_string id
    | Abstraction (arg, body) ->
      Printf.sprintf "\\%s . %s" (Id.to_string arg) (to_string body)
    | Application (fn, arg) ->
      let fn_to_string tm =
        match tm.desc with
          | Variable _ | Application _ ->
            to_string tm
          | Abstraction _ ->
            to_paren_string tm
      in
      let arg_to_string tm =
        match tm.desc with
          | Variable _ ->
            to_string tm
          | Abstraction _ | Application _ ->
            to_paren_string tm
      in
      Printf.sprintf "%s %s" (fn_to_string fn) (arg_to_string arg)

(* Constructors *)

let var ?(loc = Loc.dummy) id = var loc id

let abs ?(loc = Loc.dummy) arg body = abs loc arg body

let abs' ?(loc = Loc.dummy) args body = List.fold_right (abs ~loc) args body

let app ?(loc = Loc.dummy) fn arg = app loc fn arg

let app' ?(loc = Loc.dummy) fn args = List.fold_left (app ~loc) fn args
