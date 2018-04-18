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
let b = Type.var "B"
let c = Type.var "C"

(* Functions *)

let id = Term.tp_abs "A" (Term.abs "x" a (Term.var "x"))

let id_tp = Type.forall "A" (Type.func a a)

let app =
  Term.tp_abs'
    ["A"; "B"]
    (Term.abs'
      ["f", Type.func a b; "x", a]
      (Term.app (Term.var "f") (Term.var "x")))

let app_tp = Type.forall' ["A"; "B"] (Type.func' [Type.func a b; a] b)

let rev_app =
  Term.tp_abs'
    ["A"; "B"]
    (Term.abs'
      ["x", a; "f", Type.func a b]
      (Term.app (Term.var "f") (Term.var "x")))

let rev_app_tp =
  Type.forall' ["A"; "B"] (Type.func' [a; Type.func a b] b)

let compose =
  Term.tp_abs'
    ["A"; "B"; "C"]
    (Term.abs'
      ["f", Type.func a b; "g", Type.func c a; "x", c]
      (Term.app
        (Term.var "f")
        (Term.app (Term.var "g") (Term.var "x"))))

let compose_tp =
  Type.forall'
    ["A"; "B"; "C"]
    (Type.func' [Type.func a b; Type.func c a; c] b)

let arg_swap =
  Term.tp_abs'
    ["A"; "B"; "C"]
    (Term.abs'
      ["f", Type.func' [a; b] c; "x", b; "y", a]
      (Term.app' (Term.var "f") [Term.var "y"; Term.var "x"]))

let arg_swap_tp =
  Type.forall'
    ["A"; "B"; "C"]
    (Type.func' [Type.func' [a; b] c; b; a] c)

(* Church booleans *)

let bool_tp = Type.forall "A" @@ Type.func' [a; a] a

let fls = Term.tp_abs "A" @@ Term.abs' ["x", a; "y", a] (Term.var "y")

let tru = Term.tp_abs "A" @@ Term.abs' ["x", a; "y", a] (Term.var "x")

let not_ =
  Term.abs
    "p" bool_tp
    (Term.app' (Term.tp_app (Term.var "p") bool_tp) [fls; tru])

let and_ =
  Term.abs'
    ["p", bool_tp; "q", bool_tp]
    (Term.app' (Term.tp_app (Term.var "p") bool_tp) [Term.var "q"; fls])

let or_ =
  Term.abs'
    ["p", bool_tp; "q", bool_tp]
    (Term.app' (Term.tp_app (Term.var "p") bool_tp) [tru; Term.var "q"])

let if_ =
  Term.tp_abs "A" @@
    Term.abs'
      ["p", bool_tp; "x", a; "y", a]
      (Term.app'
        (Term.tp_app (Term.var "p") a)
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

(* Church pairs *)

let pair =
  Term.tp_abs'
    ["A"; "B"]
    (Term.abs'
      ["f", a; "s", b]
      (Term.tp_abs
        "C"
        (Term.abs
          "p" (Type.func' [a; b] c)
        (Term.app' (Term.var "p") [Term.var "f"; Term.var "s"]))))

let fst =
  Term.tp_abs'
    ["A"; "B"]
    (Term.abs
      "p" (Type.forall "C" (Type.func (Type.func' [a; b] c) c))
      (Term.app
        (Term.tp_app (Term.var "p") a)
        (Term.abs' ["f", a; "s", b] (Term.var "f"))))

let snd =
  Term.tp_abs'
    ["A"; "B"]
    (Term.abs
      "p" (Type.forall "C" (Type.func (Type.func' [a; b] c) c))
      (Term.app
        (Term.tp_app (Term.var "p") b)
        (Term.abs' ["f", a; "s", b] (Term.var "s"))))

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
      let exp_tp =
        Type.forall "A" @@
          Type.func' [Type.forall "A" @@ Type.func' [a; a] a; a; a] a
      in
      assert_equal if_ exp_tp if_) ;

    ("bool_to_nat", fun _ ->
      let exp_tm =
        Term.abs'
          ["b", bool_tp]
          (Term.app'
            (Term.tp_app (Term.var "b") nat_tp) [one; zero])
      in
      assert_equal bool_to_nat (Type.func bool_tp nat_tp) exp_tm) ;

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
      let tp = Type.func' [nat_tp; nat_tp; nat_tp] nat_tp in
      assert_equal if_zero tp if_zero) ;

  ] ) ;

  ( "Church Pairs", [

    ("pair", fun _ ->
      let exp_tp =
        Type.forall'
          ["A"; "B"]
          (Type.func'
            [a; b]
            (Type.forall "C" (Type.func (Type.func' [a; b] c) c)))
      in
      assert_equal pair exp_tp pair) ;

    ("fst", fun _ ->
      let exp_tp =
        Type.forall'
          ["A"; "B"]
          (Type.func
            (Type.forall "C" (Type.func (Type.func' [a; b] c) c))
            a)
      in
      assert_equal fst exp_tp fst) ;

    ("snd", fun _ ->
      let exp_tp =
        Type.forall'
          ["A"; "B"]
          (Type.func
            (Type.forall "C" (Type.func (Type.func' [a; b] c) c))
            b)
      in
      assert_equal snd exp_tp snd) ;

  ] ) ;

]

let beta_reduce_tests = "Term.beta_reduce", [

  ( "Functions", [

    ("id eight", fun _ ->
      let tm = Term.app (Term.tp_app id nat_tp) eight in
      assert_equal tm nat_tp eight) ;

    ("app not false", fun _ ->
      let tm =
        Term.app' (Term.tp_app' app [bool_tp; bool_tp]) [not_; fls]
      in
      assert_equal tm bool_tp tru) ;

    ("rev_app bool_to_nat true", fun _ ->
      let tm =
        Term.app'
          (Term.tp_app' rev_app [bool_tp; nat_tp])
          [tru; bool_to_nat]
      in
      assert_equal tm nat_tp one) ;

    ("compose succ bool_to_nat 2", fun _ ->
      let tm =
        Term.app'
          (Term.tp_app' compose [nat_tp; nat_tp; bool_tp])
          [succ; bool_to_nat; tru]
      in
      assert_equal tm nat_tp two) ;

    ("arg_swap (if_ (true && false)) (succ two) four", fun _ ->
      let tm =
        Term.app'
          (Term.tp_app' arg_swap [nat_tp; nat_tp; nat_tp])
          [
            Term.app
              (Term.tp_app if_ nat_tp)
              (Term.app' and_ [tru; fls]) ;
            Term.app succ two ;
            four
          ]
      in
      assert_equal tm nat_tp three) ;

  ] ) ;

  ( "Church Booleans", [

    ("not false", fun _ ->
      assert_equal (Term.app not_ fls) bool_tp tru) ;

    ("not true", fun _ ->
      assert_equal (Term.app not_ tru) bool_tp fls) ;

    ("false && false", fun _ ->
      assert_equal (Term.app' and_ [fls; fls]) bool_tp fls) ;

    ("false && true", fun _ ->
      assert_equal (Term.app' and_ [fls; tru]) bool_tp fls) ;

    ("true && false", fun _ ->
      assert_equal (Term.app' and_ [tru; fls]) bool_tp fls) ;

    ("true && true", fun _ ->
      assert_equal (Term.app' and_ [tru; tru]) bool_tp tru) ;

    ("false || false", fun _ ->
      assert_equal (Term.app' or_ [fls; fls]) bool_tp fls) ;

    ("false || true", fun _ ->
      assert_equal (Term.app' or_ [fls; tru]) bool_tp tru) ;

    ("true || false", fun _ ->
      assert_equal (Term.app' or_ [tru; fls]) bool_tp tru) ;

    ("true || true", fun _ ->
      assert_equal (Term.app' or_ [tru; tru]) bool_tp tru) ;

    ("if false false true", fun _ ->
      let tm = Term.app' (Term.tp_app if_ bool_tp) [fls; fls; tru] in
      assert_equal tm bool_tp tru) ;

    ("if true false true", fun _ ->
      let tm = Term.app' (Term.tp_app if_ bool_tp) [tru; fls; tru] in
      assert_equal tm bool_tp fls) ;

    ("if true 3 4", fun _ ->
      let tm = Term.app' (Term.tp_app if_ nat_tp) [tru; three; four] in
      assert_equal tm nat_tp three) ;

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

  ( "Church Pairs", [

    ("pair 0 1", fun _ ->
      let tm =
        Term.app' (Term.tp_app' pair [nat_tp; nat_tp]) [zero; one]
      in
      let exp_tp =
        Type.forall "C" (Type.func (Type.func' [nat_tp; nat_tp] c) c)
      in
      let exp_tm =
        Term.tp_abs
          "C"
          (Term.abs
            "p" (Type.func' [nat_tp; nat_tp] c)
            (Term.app' (Term.var "p") [zero; one]))
      in
      assert_equal tm exp_tp exp_tm) ;

    ("fst (pair 0 1)", fun _ ->
      let tm =
        Term.app
          (Term.tp_app' fst [nat_tp; nat_tp])
          (Term.app' (Term.tp_app' pair [nat_tp; nat_tp]) [zero; one])
      in
      assert_equal tm nat_tp zero) ;

    ("snd (pair 0 1)", fun _ ->
      let tm =
        Term.app
          (Term.tp_app' snd [nat_tp; nat_tp])
          (Term.app' (Term.tp_app' pair [nat_tp; nat_tp]) [zero; one])
      in
      assert_equal tm nat_tp one) ;

    ("pair false true", fun _ ->
      let tm =
        Term.app' (Term.tp_app' pair [bool_tp; bool_tp]) [fls; tru]
      in
      let bool_pair_tp =
        Type.forall "C" (Type.func (Type.func' [bool_tp; bool_tp] c) c)
      in
      let exp_tm =
        Term.tp_abs
          "C"
          (Term.abs
            "p" (Type.func' [bool_tp; bool_tp] c)
            (Term.app' (Term.var "p") [fls; tru]))
      in
      assert_equal tm bool_pair_tp exp_tm) ;

    ("fst (pair false true)", fun _ ->
      let tm =
        Term.app
          (Term.tp_app' fst [bool_tp; bool_tp])
          (Term.app' (Term.tp_app' pair [bool_tp; bool_tp]) [fls; tru])
      in
      assert_equal tm bool_tp fls) ;

    ("snd (pair false true)", fun _ ->
      let tm =
        Term.app
          (Term.tp_app' snd [bool_tp; bool_tp])
          (Term.app' (Term.tp_app' pair [bool_tp; bool_tp]) [fls; tru])
      in
      assert_equal tm bool_tp tru) ;


    ("pair false zero", fun _ ->
      let tm =
        Term.app' (Term.tp_app' pair [bool_tp; nat_tp]) [fls; zero]
      in
      let bool_nat_pair_tp =
        Type.forall "C" (Type.func (Type.func' [bool_tp; nat_tp] c) c)
      in
      let exp_tm =
        Term.tp_abs
          "C"
          (Term.abs
            "p" (Type.func' [bool_tp; nat_tp] c)
            (Term.app' (Term.var "p") [fls; zero]))
      in
      assert_equal tm bool_nat_pair_tp exp_tm) ;

    ("fst (pair false zero)", fun _ ->
      let tm =
        Term.app
          (Term.tp_app' fst [bool_tp; nat_tp])
          (Term.app' (Term.tp_app' pair [bool_tp; nat_tp]) [fls; zero])
      in
      assert_equal tm bool_tp fls) ;

    ("snd (pair false zero)", fun _ ->
      let tm =
        Term.app
          (Term.tp_app' snd [bool_tp; nat_tp])
          (Term.app' (Term.tp_app' pair [bool_tp; nat_tp]) [fls; zero])
      in
      assert_equal tm nat_tp zero) ;

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
