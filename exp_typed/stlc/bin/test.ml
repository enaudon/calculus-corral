open Stlc
open OUnit

module Id = Identifier
module Loc = Location

let assert_equal tm exp_tp act_tp =
  let msg = Printf.sprintf "For term: '%s'" (Term.to_string tm) in
  let cmp = Type.equals in
  let printer tp = Printf.sprintf "'%s'" @@ Type.to_string tp in
  assert_equal ~msg ~cmp ~printer exp_tp act_tp

let a = Type.base "A"
let b = Type.base "B"
let c = Type.base "C"
let d = Type.base "D"
let e = Type.base "E"
let f = Type.base "F"
let g = Type.base "G"
let h = Type.base "H"
let i = Type.base "I"

let tests = [

  ( "Functions", [

    (
      "id", fun _ ->
        let tm = Term.abs "x" a (Term.var "x") in
        let exp_tp = Type.func a a in
        let act_tp = Term.to_type tm in
        assert_equal tm exp_tp act_tp
    ) ;

    (
      "app", fun _ ->
        let tm =
          Term.abs'
            ["f", Type.func a b; "x", a]
            (Term.app (Term.var "f") (Term.var "x"))
        in
        let exp_tp = Type.func' [Type.func a b; a] b in
        let act_tp = Term.to_type tm in
        assert_equal tm exp_tp act_tp
    ) ;

    (
      "rev_app", fun _ ->
        let tm =
          Term.abs'
            ["x", a; "f", Type.func a b]
            (Term.app (Term.var "f") (Term.var "x"))
        in
        let exp_tp = Type.func' [a; Type.func a b] b in
        let act_tp = Term.to_type tm in
        assert_equal tm exp_tp act_tp
    ) ;

    (
      "compose", fun _ ->
        let tm =
          Term.abs'
            ["f", Type.func a b; "g", Type.func c a; "x", c]
            (Term.app
              (Term.var "f")
              (Term.app (Term.var "g") (Term.var "x")))
        in
        let exp_tp = Type.func' [Type.func a b; Type.func c a; c] b in
        let act_tp = Term.to_type tm in
        assert_equal tm exp_tp act_tp
    ) ;

    (
      "arg_swap", fun _ ->
        let tm =
          Term.abs'
            ["f", Type.func' [a; b] c; "x", b; "y", a]
            (Term.app' (Term.var "f") [Term.var "y"; Term.var "x"])
        in
        let exp_tp = Type.func' [Type.func' [a; b] c; b; a] c in
        let act_tp = Term.to_type tm in
        assert_equal tm exp_tp act_tp
    ) ;

  ] ) ;

  ( "Church Booleans", [

    (
      "false", fun _ ->
        let tm = Term.abs' ["a", a; "b", b] (Term.var "a") in
        let exp_tp = Type.func' [a; b] a in
        let act_tp = Term.to_type tm in
        assert_equal tm exp_tp act_tp
    ) ;

    (
      "false", fun _ ->
        let tm = Term.abs' ["a", a; "b", b] (Term.var "b") in
        let exp_tp = Type.func' [a; b] b in
        let act_tp = Term.to_type tm in
        assert_equal tm exp_tp act_tp
    ) ;

    (
      "if", fun _ ->
        let tm =
          Term.abs'
            ["p", Type.func' [a; b] c; "a", a; "b", b]
            (Term.app' (Term.var "p") [Term.var "a"; Term.var "b"])
        in
        let exp_tp = Type.func' [Type.func' [a; b] c; a; b] c in
        let act_tp = Term.to_type tm in
        assert_equal tm exp_tp act_tp
    ) ;

  ] ) ;

  ( "Church Naturals", [

    (
      "zero", fun _ ->
        let tm = Term.abs' ["f", a; "x", b] (Term.var "x") in
        let exp_tp = Type.func' [a; b] b in
        let act_tp = Term.to_type tm in
        assert_equal tm exp_tp act_tp
    ) ;

    (
      "one", fun _ ->
        let tm =
          Term.abs'
            ["f", Type.func a b; "x", a]
            (Term.app (Term.var "f") (Term.var "x"))
        in
        let exp_tp = Type.func' [Type.func a b; a] b in
        let act_tp = Term.to_type tm in
        assert_equal tm exp_tp act_tp
    ) ;

    (
      "two", fun _ ->
        let tm =
          Term.abs'
            ["f", Type.func a a; "x", a]
            (Term.app
              (Term.var "f")
              (Term.app (Term.var "f") (Term.var "x")))
        in
        let exp_tp = Type.func' [Type.func a a; a] a in
        let act_tp = Term.to_type tm in
        assert_equal tm exp_tp act_tp
    ) ;

    (
      "three", fun _ ->
        let tm =
          Term.abs'
            ["f", Type.func a a; "x", a]
            (Term.app
              (Term.var "f")
              (Term.app
                (Term.var "f")
                (Term.app
                  (Term.var "f")
                  (Term.var "x"))))
        in
        let exp_tp = Type.func' [Type.func a a; a] a in
        let act_tp = Term.to_type tm in
        assert_equal tm exp_tp act_tp
    ) ;

    (
      "succ", fun _ ->
        let tm =
          Term.abs'
            [
              "n", Type.func' [Type.func a b; c] a ;
              "f", Type.func a b ;
              "x", c ;
            ]
            (Term.app
              (Term.var "f")
              (Term.app' (Term.var "n") [Term.var "f"; Term.var "x"]))
        in
        let exp_tp =
          Type.func'
            [Type.func' [Type.func a b; c] a; Type.func a b; c]
            b
        in
        let act_tp = Term.to_type tm in
        assert_equal tm exp_tp act_tp
    ) ;

    (
      "pred", fun _ ->
        let tm =
          Term.abs'
            [
              ( "n",
                Type.func'
                  [
                    Type.func' [Type.func a b; Type.func b c] c ;
                    Type.func d e ;
                    Type.func f f ;
                  ]
                  g ) ;
              "f", a ;
              "x", e ;
            ]
            (Term.app'
              (Term.var "n")
              [
                Term.abs'
                  ["g", Type.func a b; "h", Type.func b c]
                  (Term.app
                    (Term.var "h")
                    (Term.app (Term.var "g") (Term.var "f"))) ;
                Term.abs "u" d (Term.var "x") ;
                Term.abs "u" f (Term.var "u") ;
              ])
        in
        let exp_tp =
          Type.func'
            [
              Type.func'
                [
                  Type.func' [Type.func a b; Type.func b c] c ;
                  Type.func d e ;
                  Type.func f f ;
                ]
                g ;
              a ;
              e ;
            ]
            g
        in
        let act_tp = Term.to_type tm in
        assert_equal tm exp_tp act_tp
    ) ;

    (
      "plus", fun _ ->
        let tm =
          Term.abs'
            [
              "m", Type.func a b ;
              "n", Type.func' [Type.func c a; d] c ;
              "f", Type.func c a ;
              "x", d ;
            ]
            (Term.app
              (Term.var "m")
              (Term.app
                (Term.var "f")
                (Term.app'
                  (Term.var "n")
                  [Term.var "f"; Term.var "x"])))
        in
        let exp_tp =
          Type.func'
            [
              Type.func a b ;
              Type.func' [Type.func c a; d] c ;
              Type.func c a ;
              d ;
            ]
            b
        in
        let act_tp = Term.to_type tm in
        assert_equal tm exp_tp act_tp
    ) ;

    (
      "minus", fun _ ->
        let tm =
          Term.abs'
            [
              "m", a ;
              ( "n", 
                Type.func'
                [
                  Type.func'
                    [
                      Type.func'
                        [
                          Type.func' [Type.func b c; Type.func c d] d ;
                          Type.func e f ;
                          Type.func g g ;
                        ]
                        h ;
                      b ;
                      f ;
                    ]
                    h ;
                  a ;
                ]
                i ) ;
            ]
            (Term.app'
              (Term.var "n")
              [
                Term.abs'
                  [
                    ( "n",
                      Type.func'
                        [
                          Type.func' [Type.func b c; Type.func c d] d ;
                          Type.func e f ;
                          Type.func g g ;
                        ]
                        h );
                    "f", b ;
                    "x", f ;
                  ]
                  (Term.app'
                    (Term.var "n")
                    [
                      Term.abs'
                        ["g", Type.func b c; "h", Type.func c d]
                        (Term.app
                          (Term.var "h")
                          (Term.app (Term.var "g") (Term.var "f"))) ;
                      Term.abs "u" e (Term.var "x") ;
                      Term.abs "u" g (Term.var "u") ;
                    ]) ;
                    (Term.var "m") ;
              ])
        in
        let exp_tp =
          Type.func'
            [
              a;
              Type.func'
              [
                Type.func'
                  [
                    Type.func'
                      [
                        Type.func' [Type.func b c; Type.func c d] d ;
                        Type.func e f ;
                        Type.func g g ;
                      ]
                      h ;
                    b ;
                    f ;
                  ]
                  h ;
                a ;
              ]
              i
            ]
            i
        in
        let act_tp = Term.to_type tm in
        assert_equal tm exp_tp act_tp
    ) ;

    (
      "mult", fun _ ->
        let tm =
          Term.abs'
            ["m", Type.func a b; "n", Type.func c a; "f", c]
            (Term.app
              (Term.var "m")
              (Term.app (Term.var "n") (Term.var "f")))
        in
        let exp_tp = Type.func' [Type.func a b; Type.func c a; c] b in
        let act_tp = Term.to_type tm in
        assert_equal tm exp_tp act_tp
    ) ;

    (
      "exp", fun _ ->
        let tm =
          Term.abs'
            ["m", a; "n", Type.func a b]
            (Term.app (Term.var "n") (Term.var "m"))
        in
        let exp_tp = Type.func' [a; Type.func a b] b in
        let act_tp = Term.to_type tm in
        assert_equal tm exp_tp act_tp
    ) ;

  ] ) ;

]

let make_test_suite tests =
  let mapper (name, tests) =
    name >:::
      List.map (fun (name, fn) -> Id.reset (); name >:: fn) tests
  in
  "Tests" >::: List.map mapper tests

let _ = run_test_tt_main (make_test_suite tests)
