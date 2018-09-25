module Id = Identifier

open OUnit

module Type = struct

  include Universal_types.Type

  let var id = var @@ Id.of_string id

  let forall quant body = forall (Id.of_string quant) body

end

module Term = struct

  include Universal_types.Term

  let var id = var (Id.of_string id)

  let abs arg tp body = abs (Id.of_string arg) tp body

  let app fn arg = app fn arg

  let tp_abs arg body = tp_abs (Id.of_string arg) body

  let tp_app fn arg = tp_app fn arg

end

let assert_beta_reduce tm exp_shallow exp_deep =

  let assert_beta_reduce ?deep tm exp =
    let act =
      try
        Term.beta_reduce ?deep Id.Map.empty tm
      with Failure msg ->
        assert_failure @@
          Printf.sprintf "Failure beta-reducing '%s'\n%s"
          (Term.to_string tm)
          msg
    in
    let msg = Printf.sprintf "For term: '%s'" (Term.to_string tm) in
    let cmp = Term.alpha_equivalent in
    let printer tp = Printf.sprintf "'%s'" @@ Term.to_string tp in
    assert_equal ~msg ~cmp ~printer exp act
  in

  assert_beta_reduce tm exp_shallow;
  assert_beta_reduce ~deep:() tm exp_deep

let assert_alpha_equivalent tm1 tm2 exp =
  let act = Term.alpha_equivalent tm1 tm2 in
  let msg =
    Printf.sprintf
      "For terms `%s` and `%s`\nExpect: %b\nActual %b\n"
      (Term.to_string tm1)
      (Term.to_string tm2)
      exp
      act
  in
  assert_bool msg (exp = act)

(* Utility functions *)

let a = Type.var "A"

let b = Type.var "B"

let id_tp = Type.forall "A" (Type.func a a)

let id v = Term.tp_abs "A" (Term.abs v a @@ Term.var v)

let id_fn v = Term.tp_app (id v) id_tp

let alpha_equivalent_tests = "alpha_equivalent", [

  ( "x = x", fun _ ->
    let tm = Term.var "x" in
    assert_alpha_equivalent tm tm true ) ;

  ( "x <> y", fun _ ->
    assert_alpha_equivalent (Term.var "x") (Term.var "y") false ) ;

  ( "id = id", fun _ ->
    let tm = id "x" in
    assert_alpha_equivalent tm tm true ) ;

  ( "id = id'", fun _ ->
    assert_alpha_equivalent (id "x") (id "y") true ) ;

  ( "id_fn id = id_fn id", fun _ ->
    let tm = Term.app (id_fn "x") (id "x") in
    assert_alpha_equivalent tm tm true ) ;

  ( "id_fn id = id_fn' id'", fun _ ->
    let tm1 = Term.app (id_fn "x") (id "x") in
    let tm2 = Term.app (id_fn "y") (id "y") in
    assert_alpha_equivalent tm1 tm2 true ) ;

  ( "id_fn <> id", fun _ ->
    assert_alpha_equivalent (id_fn "x") (id "x") false ) ;

  ( "\\A . \\y : A . x = \\A . \\y : A . x", fun _ ->
    let tm = Term.tp_abs "A" (Term.abs "y" a @@ Term.var "x") in
    assert_alpha_equivalent tm tm true ) ;

  ( "\\A . \\y : A . x <> \\A . \\x : A . x", fun _ ->
    let tm1 = Term.tp_abs "A" (Term.abs "y" a @@ Term.var "x") in
    let tm2 = Term.tp_abs "A" (Term.abs "x" a @@ Term.var "x") in
    assert_alpha_equivalent tm1 tm2 false ) ;

  ( "\\A . \\y : B . y = \\A . \\y : A . x", fun _ ->
    let tm1 = Term.tp_abs "A" (Term.abs "y" b @@ Term.var "y") in
    let tm2 = Term.tp_abs "A" (Term.abs "x" a @@ Term.var "x") in
    assert_alpha_equivalent tm1 tm2 false ) ;

]

let beta_reduce_tests = "beta_reduce", [

  ( "x", fun _ ->
    let tm = Term.var "x" in
    assert_beta_reduce tm tm tm ) ;

  ( "id", fun _ ->
    let tm = id "x" in
    assert_beta_reduce tm tm tm ) ;

  ( "id_fn id", fun _ ->
    let tm = id "x" in
    let redux = Term.app (id_fn "x") tm in
    assert_beta_reduce redux tm tm ) ;

  ( "\\A . \\x : A . id_fn id", fun _ ->
    let redux =
      Term.tp_abs "A" (Term.abs "x" a @@ Term.app (id_fn "x") (id "x"))
    in
    let exp_deep = Term.tp_abs "A" (Term.abs "x" a @@ id "x") in
    assert_beta_reduce redux redux exp_deep ) ;

]

let make_test_suite (name, tests) =
  let mapper (name, test) = Id.reset (); name >:: test in
  name >::: List.map mapper tests

let make () = "Term" >::: [
  make_test_suite alpha_equivalent_tests ;
  make_test_suite beta_reduce_tests ;
]
