open Stlc
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
  let cmp = Type.alpha_equivalent in
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

let b = Type.base "B"

(* Church naturals *)

let nat_tp = Type.func' [Type.func b b; b] b

let zero = Term.abs' ["s", Type.func b b; "z", b] (Term.var "z")

let one =
  Term.abs'
    ["s", Type.func b b; "z", b]
    (Term.app (Term.var "s") (Term.var "z"))

let two =
  Term.abs'
    ["s", Type.func b b; "z", b]
    (Term.app (Term.var "s") (Term.app (Term.var "s") (Term.var "z")))

let three =
  Term.abs'
    ["s", Type.func b b; "z", b]
    (Term.app
      (Term.var "s")
      (Term.app
        (Term.var "s")
        (Term.app (Term.var "s") (Term.var "z"))))

let four =
  Term.abs'
    ["s", Type.func b b; "z", b]
    (Term.app
      (Term.var "s")
      (Term.app
        (Term.var "s")
        (Term.app
          (Term.var "s")
          (Term.app (Term.var "s") (Term.var "z")))))

let eight =
  Term.abs'
    ["s", Type.func b b; "z", b]
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
  Term.abs'
    ["n", nat_tp; "s", Type.func b b; "z", b]
    (Term.app
      (Term.var "s")
      (Term.app' (Term.var "n") [Term.var "s"; Term.var "z"]))

let add =
  Term.abs'
    ["m", nat_tp; "n", nat_tp; "s", Type.func b b; "z", b]
    (Term.app'
      (Term.var "m")
      [
        (Term.var "s") ;
        (Term.app'
          (Term.var "n")
          [Term.var "s"; Term.var "z"]) ;
      ])

let mul =
  Term.abs'
    ["m", nat_tp; "n", nat_tp; "s", Type.func b b]
    (Term.app (Term.var "m") (Term.app (Term.var "n") (Term.var "s")))

let if_zero =
  Term.abs'
    ["m", nat_tp; "n", nat_tp; "l", nat_tp; "s", Type.func b b; "z", b]
    (Term.app'
      (Term.var "m")
      [
        (Term.abs "x" b
          (Term.app' (Term.var "l") [Term.var "s"; Term.var "z"])) ;
        (Term.app' (Term.var "n") [Term.var "s"; Term.var "z"])
      ])

(* Church booleans *)

let bool_tp = Type.func' [nat_tp; nat_tp] nat_tp

let fls = Term.abs' ["m", nat_tp; "n", nat_tp] (Term.var "n")

let tru = Term.abs' ["m", nat_tp; "n", nat_tp] (Term.var "m")

let if_ =
  Term.abs'
    ["b", bool_tp; "m", nat_tp; "n", nat_tp]
    (Term.app' (Term.var "b") [Term.var "m"; Term.var "n"])

let bool_to_nat =
  Term.abs "b" bool_tp (Term.app' if_ [Term.var "b"; one; zero])

(* Tests *)

let to_type_tests = "Term.to_type tests", [

  ( "Functions", [

    (
      "id", fun _ ->
        let tm = Term.abs "x" b (Term.var "x") in
        assert_equal tm (Type.func b b) tm
    ) ;

    (
      "app", fun _ ->
        let tm =
          Term.abs'
            ["f", Type.func b b; "x", b]
            (Term.app (Term.var "f") (Term.var "x"))
        in
        assert_equal tm (Type.func' [Type.func b b; b] b) tm
    ) ;

    (
      "rev_app", fun _ ->
        let tm =
          Term.abs'
            ["x", b; "f", Type.func b b]
            (Term.app (Term.var "f") (Term.var "x"))
        in
        assert_equal tm (Type.func' [b; Type.func b b] b) tm
    ) ;

    (
      "compose", fun _ ->
        let tm =
          Term.abs'
            ["f", Type.func b b; "g", Type.func b b; "x", b]
            (Term.app
              (Term.var "f")
              (Term.app (Term.var "g") (Term.var "x")))
        in
        let exp_tp = Type.func' [Type.func b b; Type.func b b; b] b in
        assert_equal tm exp_tp tm
    ) ;

    (
      "arg_swap", fun _ ->
        let tm =
          Term.abs'
            ["f", Type.func' [b; b] b; "x", b; "y", b]
            (Term.app' (Term.var "f") [Term.var "y"; Term.var "x"])
        in
        assert_equal tm (Type.func' [Type.func' [b; b] b; b; b] b) tm
    ) ;

  ] ) ;

  ( "Church Naturals", [

    ("zero", fun _ -> assert_equal zero nat_tp zero) ;

    ("one", fun _ -> assert_equal one nat_tp one) ;

    ("two", fun _ -> assert_equal two nat_tp two) ;

    ("three", fun _ -> assert_equal three nat_tp three) ;

    ("four", fun _ -> assert_equal four nat_tp four) ;

    ("eight", fun _ -> assert_equal eight nat_tp eight) ;

    (
      "succ", fun _ -> assert_equal succ (Type.func nat_tp nat_tp) succ
    ) ;

    (
      "add", fun _ ->
        assert_equal add (Type.func' [nat_tp; nat_tp] nat_tp) add
    ) ;

    (
      "mul", fun _ ->
        assert_equal mul (Type.func' [nat_tp; nat_tp] nat_tp) mul
    ) ;

    (
      "if_zero", fun _ ->
        assert_equal
          if_zero
          (Type.func' [nat_tp; nat_tp; nat_tp] nat_tp)
          if_zero
    ) ;

  ] ) ;

  ( "Church Booleans", [

    ("false", fun _ -> assert_equal fls bool_tp fls) ;

    ("true", fun _ -> assert_equal tru bool_tp tru) ;

    (
      "if", fun _ ->
        let exp_tp = Type.func' [bool_tp; nat_tp; nat_tp] nat_tp in
        assert_equal if_ exp_tp if_
    ) ;

    (
      "bool_to_nat", fun _ ->
        assert_equal
          bool_to_nat
          (Type.func bool_tp nat_tp)
          (Term.abs "b" bool_tp @@ Term.app' (Term.var "b") [one; zero])
    ) ;

  ] ) ;

]

let beta_reduce_tests = "Term.beta_reduce", [

  ( "Church Naturals", [

    (
      "succ one", fun _ -> assert_equal (Term.app succ one) nat_tp two
    ) ;

    (
      "1 + 0", fun _ ->
        assert_equal (Term.app' add [one; zero]) nat_tp one
    ) ;

    (
      "1 + 2", fun _ ->
        assert_equal (Term.app' add [one; two]) nat_tp three
    ) ;

    (
      "2 * 2", fun _ ->
        assert_equal (Term.app' mul [two; two]) nat_tp four
    ) ;

    (
      "2 * 0", fun _ ->
        assert_equal (Term.app' mul [two; zero]) nat_tp zero
    ) ;

    (
      "2 * 2 * 2", fun _ ->
        let tm = Term.app' mul [two; Term.app' mul [two; two]] in
        assert_equal tm nat_tp eight
    ) ;

    (
      "if_zero 0 1 2", fun _ ->
        assert_equal (Term.app' if_zero [zero; one; two]) nat_tp one
    ) ;

    (
      "if_zero 1 1 2", fun _ ->
        assert_equal (Term.app' if_zero [one; one; two]) nat_tp two
    ) ;

    (
      "if_zero 2 1 2", fun _ ->
        assert_equal (Term.app' if_zero [two; one; two]) nat_tp two
    ) ;

    (
      "if_zero (4 * 4 + 1) (8 * 2) (1 + 2 + 1)", fun _ ->
        let tm =
          Term.app'
            if_zero
            [
              Term.app' add [Term.app' mul [four; four]; one] ;
              Term.app' mul [eight; two] ;
              Term.app' add [one; Term.app' add [two; one]] ;
            ]
        in
        assert_equal tm nat_tp four
    ) ;

    (
      (* [add one] should be beta-equivalent to [succ] *)
      "add-one = succ", fun _ ->
        assert_equal (Term.app add one) (Type.func nat_tp nat_tp) succ
    ) ;

  ] ) ;

  ( "Church Booleans", [

    (
      "bool_to_nat false", fun _ ->
        assert_equal (Term.app bool_to_nat fls) nat_tp zero
    ) ;

    (
      "bool_to_nat true", fun _ ->
        assert_equal (Term.app bool_to_nat tru) nat_tp one
    ) ;

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
