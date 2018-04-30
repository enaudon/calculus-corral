open Oper
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

let b = Type.base

(* Functions *)

let id = Term.abs "x" b (Term.var "x")

let id_tp = Type.func b b

let app =
  Term.abs'
    ["f", Type.func b b; "x", b]
    (Term.app (Term.var "f") (Term.var "x"))

let app_tp = Type.func' [Type.func b b; b] b

let rev_app =
  Term.abs'
    ["x", b; "f", Type.func b b]
    (Term.app (Term.var "f") (Term.var "x"))

let rev_app_tp = Type.func' [b; Type.func b b] b

let compose =
  Term.abs'
    ["f", Type.func b b; "g", Type.func b b; "x", b]
    (Term.app
      (Term.var "f")
      (Term.app (Term.var "g") (Term.var "x")))

let compose_tp = Type.func' [Type.func b b; Type.func b b; b] b

let arg_swap =
  Term.abs'
    ["f", Type.func' [b; b] b; "x", b; "y", b]
    (Term.app' (Term.var "f") [Term.var "y"; Term.var "x"])

let arg_swap_tp = Type.func' [Type.func' [b; b] b; b; b] b

(* Church booleans *)

let bool_tp = Type.func' [b; b] b

let fls = Term.abs' ["x", b; "y", b] (Term.var "y")

let tru = Term.abs' ["x", b; "y", b] (Term.var "x")

let if_ =
  Term.abs'
    ["p", bool_tp; "m", b; "n", b]
    (Term.app' (Term.var "p") [Term.var "m"; Term.var "n"])

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

(* Church pairs *)

let pair_tp =
  Type.abs'
    ["A", Kind.base; "B", Kind.base; "C", Kind.base]
    (Type.func
      (Type.func' [Type.var "A"; Type.var "B"] (Type.var "C"))
      (Type.var "C"))

let bool_pair_tp = Type.app' pair_tp [bool_tp; bool_tp; bool_tp]

let bool_pair =
  Term.abs'
    [
      "f", bool_tp ;
      "s", bool_tp ;
      "p", Type.func' [bool_tp; bool_tp] bool_tp ;
    ]
    (Term.app' (Term.var "p") [Term.var "f"; Term.var "s"])

let bool_pair_fst =
  Term.abs
    "p" bool_pair_tp
    (Term.app
      (Term.var "p")
      (Term.abs' ["f", bool_tp; "s", bool_tp] (Term.var "f")))

let bool_pair_snd =
  Term.abs
    "p" bool_pair_tp
    (Term.app
      (Term.var "p")
      (Term.abs' ["f", bool_tp; "s", bool_tp] (Term.var "s")))

let nat_pair_tp = Type.app' pair_tp [nat_tp; nat_tp; nat_tp]

let nat_pair =
  Term.abs'
    ["f", nat_tp; "s", nat_tp; "p", Type.func' [nat_tp; nat_tp] nat_tp]
    (Term.app' (Term.var "p") [Term.var "f"; Term.var "s"])

let nat_pair_fst =
  Term.abs
    "p" nat_pair_tp
    (Term.app
      (Term.var "p")
      (Term.abs' ["f", nat_tp; "s", nat_tp] (Term.var "f")))

let nat_pair_snd =
  Term.abs
    "p" nat_pair_tp
    (Term.app
      (Term.var "p")
      (Term.abs' ["f", nat_tp; "s", nat_tp] (Term.var "s")))

(* Tests *)

let to_type_tests = "Term.to_type tests", [

  ( "Functions", [

    ("id", fun _ -> assert_equal id id_tp id) ;

    ("app", fun _ -> assert_equal app app_tp app) ;

    ("rev_app", fun _ ->
      assert_equal rev_app rev_app_tp rev_app) ;

    ("compose", fun _ ->
      assert_equal compose compose_tp compose) ;

    ("arg_swap", fun _ ->
      assert_equal arg_swap arg_swap_tp arg_swap) ;

  ] ) ;

  ( "Church Booleans", [

    ("false", fun _ -> assert_equal fls bool_tp fls) ;

    ("true", fun _ -> assert_equal tru bool_tp tru) ;

    ("if", fun _ ->
        let exp_tp = Type.func' [bool_tp; b; b] b in
        assert_equal if_ exp_tp if_) ;

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
      let exp_tp = Type.func' [nat_tp; nat_tp; nat_tp] nat_tp in
      assert_equal if_zero exp_tp if_zero) ;

  ] ) ;

  ( "Church Pairs", [

    ("nat_pair", fun _ ->
      let exp_tp = Type.func' [nat_tp; nat_tp] nat_pair_tp in
      assert_equal nat_pair exp_tp nat_pair) ;

    ("nat_pair_fst", fun _ ->
      let exp_tp = Type.func nat_pair_tp nat_tp in
      assert_equal nat_pair_fst exp_tp nat_pair_fst) ;

    ("nat_pair_snd", fun _ ->
      let exp_tp = Type.func nat_pair_tp nat_tp in
      assert_equal nat_pair_snd exp_tp nat_pair_snd) ;

    ("bool_pair", fun _ ->
      let exp_tp = Type.func' [bool_tp; bool_tp] bool_pair_tp in
      assert_equal bool_pair exp_tp bool_pair) ;

    ("bool_pair_fst", fun _ ->
      let exp_tp = Type.func bool_pair_tp bool_tp in
      assert_equal bool_pair_fst exp_tp bool_pair_fst) ;

    ("bool_pair_snd", fun _ ->
      let exp_tp = Type.func bool_pair_tp bool_tp in
      assert_equal bool_pair_snd exp_tp bool_pair_snd) ;

  ] ) ;

]

let beta_reduce_tests = "Term.beta_reduce", [

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

  ( "Church Pairs", [

    ("nat_pair 0 1", fun _ ->
      let tm = Term.app' nat_pair [zero; one] in
      let exp_tm =
        Term.abs
          "p" (Type.func' [nat_tp; nat_tp] nat_tp)
          (Term.app' (Term.var "p") [zero; one])
      in
      assert_equal tm nat_pair_tp exp_tm) ;

    ("nat_pair_fst (nat_pair 0 1)", fun _ ->
      let tm = Term.app nat_pair_fst (Term.app' nat_pair [zero; one]) in
      assert_equal tm nat_tp zero) ;

    ("nat_pair_snd (nat_pair 0 1)", fun _ ->
      let tm = Term.app nat_pair_snd (Term.app' nat_pair [zero; one]) in
      assert_equal tm nat_tp one) ;

    ("bool_pair false true", fun _ ->
      let tm = Term.app' bool_pair [fls; tru] in
      let exp_tm =
        Term.abs
          "p" (Type.func' [bool_tp; bool_tp] bool_tp)
          (Term.app' (Term.var "p") [fls; tru])
      in
      assert_equal tm bool_pair_tp exp_tm) ;

    ("bool_pair_fst (bool_pair false true)", fun _ ->
      let tm =
        Term.app bool_pair_fst (Term.app' bool_pair [fls; tru])
      in
      assert_equal tm bool_tp fls) ;

    ("bool_pair_snd (bool_pair false true)", fun _ ->
      let tm =
        Term.app bool_pair_snd (Term.app' bool_pair [fls; tru])
      in
      assert_equal tm bool_tp tru) ;

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
