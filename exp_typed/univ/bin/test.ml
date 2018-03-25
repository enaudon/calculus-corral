open Univ
open OUnit

module Id = Identifier
module Loc = Location

let assert_equal_tp tm exp_tp =
  let act_tp =
    try
      Term.to_type tm
    with Failure msg ->
      assert_failure @@
        Printf.sprintf "Failure typing '%s'\n%s"
        (Term.to_string tm)
        msg
  in
  let msg = Printf.sprintf "For term: '%s'" (Term.to_string tm) in
  let cmp tp1 tp2 = Type.alpha_equivalent tp1 tp2 in
  let printer tp = Printf.sprintf "'%s'" @@ Type.to_string tp in
  assert_equal ~msg ~cmp ~printer exp_tp act_tp

let assert_equal tm exp_tp exp_tm =
  assert_equal_tp tm exp_tp;
  let act_tm =
    try
      Term.beta_reduce ~deep:() tm
    with Failure msg ->
      assert_failure @@
        Printf.sprintf "Failure beta-reducing '%s'\n%s"
        (Term.to_string tm)
        msg
  in
  let msg = Printf.sprintf "For term: '%s'" (Term.to_string tm) in
  let cmp = Term.alpha_equivalent in
  let printer tp = Printf.sprintf "'%s'" @@ Term.to_string tp in
  assert_equal ~msg ~cmp ~printer exp_tm act_tm

let a = Type.var "A"
let b = Type.base "B"

(* Church booleans *)

let bool_tp = Type.forall "A" @@ Type.func' [a; a] a

let fls = Term.tp_abs "A" @@ Term.abs' ["x", a; "y", a] (Term.var "y")

let tru = Term.tp_abs "A" @@ Term.abs' ["x", a; "y", a] (Term.var "x")

let if_ =
  Term.tp_abs "A" @@
    Term.abs'
      ["b", bool_tp; "x", a; "y", a]
      (Term.app'
        (Term.tp_app (Term.var "b") a)
        [Term.var "x"; Term.var "y"])

(* Church naturals *)

let nat_tp = Type.forall "A" @@ Type.func' [Type.func a a; a] a

let zero =
  Term.tp_abs "A" @@
    Term.abs' ["s", Type.func a a; "z", a] (Term.var "z")

let one =
  Term.tp_abs "A" @@
    Term.abs'
      ["s", Type.func a a; "z", a]
      (Term.app (Term.var "s") (Term.var "z"))

let two =
  Term.tp_abs "A" @@
    Term.abs'
      ["s", Type.func a a; "z", a]
      (Term.app (Term.var "s") (Term.app (Term.var "s") (Term.var "z")))

let three =
  Term.tp_abs "A" @@
    Term.abs'
      ["s", Type.func a a; "z", a]
      (Term.app
        (Term.var "s")
        (Term.app
          (Term.var "s")
          (Term.app (Term.var "s") (Term.var "z"))))

let four =
  Term.tp_abs "A" @@
    Term.abs'
      ["s", Type.func a a; "z", a]
      (Term.app
        (Term.var "s")
        (Term.app
          (Term.var "s")
          (Term.app
            (Term.var "s")
            (Term.app (Term.var "s") (Term.var "z")))))

let eight =
  Term.tp_abs "A" @@
    Term.abs'
      ["s", Type.func a a; "z", a]
      (Term.app
        (Term.var "s")
        (Term.app
          (Term.var "s")
          (Term.app
            (Term.var "s")
            (Term.app
              (Term.var "s")
              (Term.app
                (Term.var "s")
                (Term.app
                  (Term.var "s")
                  (Term.app
                    (Term.var "s")
                    (Term.app (Term.var "s") (Term.var "z")))))))))

let succ =
  Term.abs
    "n" nat_tp
    (Term.tp_abs "A"
      (Term.abs'
        ["s", Type.func a a; "z", a]
        (Term.app
          (Term.var "s")
          (Term.app'
            (Term.tp_app (Term.var "n") a)
            [Term.var "s"; Term.var "z"]))))

let add =
  Term.abs'
    ["m", nat_tp; "n", nat_tp]
    (Term.tp_abs "A"
      (Term.abs'
        ["s", Type.func a a; "z", a]
        (Term.app'
          (Term.tp_app (Term.var "m") a)
          [
            (Term.var "s") ;
            (Term.app'
              (Term.tp_app (Term.var "n") a)
              [Term.var "s"; Term.var "z"]) ;
          ])))

let mul =
  Term.abs'
    ["m", nat_tp; "n", nat_tp]
    (Term.tp_abs "A"
      (Term.abs'
        ["s", Type.func a a]
        (Term.app
          (Term.tp_app (Term.var "m") a)
          (Term.app
            (Term.tp_app (Term.var "n") a)
            (Term.var "s")))))

let if_zero =
  Term.abs'
    ["m", nat_tp; "n", nat_tp; "l", nat_tp]
    (Term.tp_abs "A"
      (Term.abs'
        ["s", Type.func a a; "z", a]
        (Term.app'
          (Term.tp_app (Term.var "m") a)
          [
            (Term.abs "x" a
              (Term.app'
                (Term.tp_app (Term.var "l") a)
                [Term.var "s"; Term.var "z"])) ;
            (Term.app'
              (Term.tp_app (Term.var "n") a)
              [Term.var "s"; Term.var "z"])
          ])))

let bool_to_nat =
  Term.abs
    "b" bool_tp
    (Term.app'
      (Term.tp_app if_ nat_tp) [Term.var "b"; one; zero])

(* Tests *)

let to_type_tests = "Term.to_type tests", [

  ( "Functions", [

    ("id", fun _ ->
      let tm = Term.abs "x" b (Term.var "x") in
      assert_equal tm (Type.func b b) tm) ;

    ("app", fun _ ->
      let tm =
        Term.abs'
          ["f", Type.func b b; "x", b]
          (Term.app (Term.var "f") (Term.var "x"))
      in
      assert_equal tm (Type.func' [Type.func b b; b] b) tm) ;

    ("rev_app", fun _ ->
      let tm =
        Term.abs'
          ["x", b; "f", Type.func b b]
          (Term.app (Term.var "f") (Term.var "x"))
      in
      assert_equal tm (Type.func' [b; Type.func b b] b) tm) ;

    ("compose", fun _ ->
      let tm =
        Term.abs'
          ["f", Type.func b b; "g", Type.func b b; "x", b]
          (Term.app
            (Term.var "f")
            (Term.app (Term.var "g") (Term.var "x")))
      in
      let exp_tp = Type.func' [Type.func b b; Type.func b b; b] b in
      assert_equal tm exp_tp tm) ;

    ("arg_swap", fun _ ->
      let tm =
        Term.abs'
          ["f", Type.func' [b; b] b; "x", b; "y", b]
          (Term.app' (Term.var "f") [Term.var "y"; Term.var "x"])
      in
      assert_equal tm (Type.func' [Type.func' [b; b] b; b; b] b) tm) ;

  ] ) ;

  ( "Church Booleans", [

    ("false", fun _ -> assert_equal fls bool_tp fls) ;

    ("true", fun _ -> assert_equal tru bool_tp tru) ;

    ("if", fun _ ->
      let exp_tp =
        Type.forall "A" @@
          Type.func' [Type.forall "A" @@ Type.func' [a; a] a; a; a] a
      in
      assert_equal if_ exp_tp if_) ;

    ("bool_to_nat", fun _ ->
      let exp_tp =
        Term.abs'
          ["b", bool_tp]
          (Term.app'
            (Term.tp_app (Term.var "b") nat_tp) [one; zero])
      in
      assert_equal bool_to_nat (Type.func bool_tp nat_tp) exp_tp) ;

  ] ) ;

  ( "Church Naturals", [

    ("zero", fun _ -> assert_equal zero nat_tp zero) ;

    ("one", fun _ -> assert_equal one nat_tp one) ;

    ("two", fun _ -> assert_equal two nat_tp two) ;

    ("three", fun _ -> assert_equal three nat_tp three) ;

    ("four", fun _ -> assert_equal four nat_tp four) ;

    ("eight", fun _ -> assert_equal eight nat_tp eight) ;

    ("succ", fun _ ->
      assert_equal succ (Type.func nat_tp nat_tp) succ) ;

    ("add", fun _ ->
      assert_equal add (Type.func' [nat_tp; nat_tp] nat_tp) add) ;

    ("mul", fun _ ->
      assert_equal mul (Type.func' [nat_tp; nat_tp] nat_tp) mul) ;

    ("if_zero", fun _ ->
      assert_equal
        if_zero
        (Type.func' [nat_tp; nat_tp; nat_tp] nat_tp)
        if_zero) ;

  ] ) ;

]

let beta_reduce_tests = "Term.beta_reduce", [

  ( "Church Booleans", [

    ("bool_to_nat false", fun _ ->
      assert_equal (Term.app bool_to_nat fls) nat_tp zero) ;

    ("bool_to_nat true", fun _ ->
      assert_equal (Term.app bool_to_nat tru) nat_tp one) ;

  ] ) ;

  ( "Church Naturals", [

    ("succ one", fun _ -> assert_equal (Term.app succ one) nat_tp two) ;

    ("1 + 0", fun _ ->
      assert_equal (Term.app' add [one; zero]) nat_tp one) ;

    ("1 + 2", fun _ ->
      assert_equal (Term.app' add [one; two]) nat_tp three) ;

    ("2 * 2", fun _ ->
      assert_equal (Term.app' mul [two; two]) nat_tp four) ;

    ("2 * 0", fun _ ->
      assert_equal (Term.app' mul [two; zero]) nat_tp zero) ;

    ("2 * 2 * 2", fun _ ->
      let tm = Term.app' mul [two; Term.app' mul [two; two]] in
      assert_equal tm nat_tp eight) ;

    ("if_zero 0 1 2", fun _ ->
      assert_equal (Term.app' if_zero [zero; one; two]) nat_tp one) ;

    ("if_zero 1 1 2", fun _ ->
      assert_equal (Term.app' if_zero [one; one; two]) nat_tp two) ;

    ("if_zero 2 1 2", fun _ ->
      assert_equal (Term.app' if_zero [two; one; two]) nat_tp two) ;

    ("if_zero (4 * 4 + 1) (8 * 2) (1 + 2 + 1)", fun _ ->
      let tm =
        Term.app'
          if_zero
          [
            Term.app' add [Term.app' mul [four; four]; one] ;
            Term.app' mul [eight; two] ;
            Term.app' add [one; Term.app' add [two; one]] ;
          ]
      in
      assert_equal tm nat_tp four) ;

    (* [add one] should be beta-equivalent to [succ] *)
    ("add-one = succ", fun _ ->
      assert_equal (Term.app add one) (Type.func nat_tp nat_tp) succ) ;

  ] ) ;

]

let make_test_suite (name, tests) =
  let mapper (name, tests) =
    name >:::
      List.map (fun (name, fn) -> Id.reset (); name >:: fn) tests
  in
  name >::: List.map mapper tests

let test_suites = "Mono tests" >::: [
  make_test_suite to_type_tests ;
  make_test_suite beta_reduce_tests ;
]

let _ = run_test_tt_main test_suites
