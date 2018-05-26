open Mono
open OUnit

module Id = Identifier
module Loc = Location

let assert_equal tm exp_tp act_tp =
  let msg = Printf.sprintf "For term: '%s'" (Term.to_string tm) in
  let cmp = Type.equals in
  let printer tp =
    Printf.sprintf "'%s'" @@ Type.to_string ~no_simp:() tp
  in
  assert_equal ~msg ~cmp ~printer exp_tp act_tp

let a = Type.var @@ Id.of_string "_0"
let b = Type.var @@ Id.of_string "_1"
let c = Type.var @@ Id.of_string "_2"
let d = Type.var @@ Id.of_string "_3"
let e = Type.var @@ Id.of_string "_4"
let f = Type.var @@ Id.of_string "_5"
let g = Type.var @@ Id.of_string "_6"
let h = Type.var @@ Id.of_string "_7"
let i = Type.var @@ Id.of_string "_8"

let tests = [

  ( "Functions", [

    ( "id", fun to_type _ ->
      let tm = Term.abs "x" (Term.var "x") in
      let exp_tp = Type.func a a in
      let act_tp = Type.simplify @@ to_type tm in
      assert_equal tm exp_tp act_tp ) ;

    ( "app", fun to_type _ ->
      let tm =
        Term.abs' ["f"; "x"] (Term.app (Term.var "f") (Term.var "x"))
      in
      let exp_tp = Type.func' [Type.func a b; a] b in
      let act_tp = Type.simplify @@ to_type tm in
      assert_equal tm exp_tp act_tp ) ;

    ( "rev_app", fun to_type _ ->
      let tm =
        Term.abs' ["x"; "f"] (Term.app (Term.var "f") (Term.var "x"))
      in
      let exp_tp = Type.func' [a; Type.func a b] b in
      let act_tp = Type.simplify @@ to_type tm in
      assert_equal tm exp_tp act_tp ) ;

    ( "compose", fun to_type _ ->
      let tm =
        Term.abs'
          ["f"; "g"; "x"]
          (Term.app
            (Term.var "f")
            (Term.app (Term.var "g") (Term.var "x")))
      in
      let exp_tp = Type.func' [Type.func a b; Type.func c a; c] b in
      let act_tp = Type.simplify @@ to_type tm in
      assert_equal tm exp_tp act_tp ) ;

    ( "arg_swap", fun to_type _ ->
      let tm =
        Term.abs'
          ["f"; "x"; "y"]
          (Term.app' (Term.var "f") [Term.var "y"; Term.var "x"])
      in
      let exp_tp = Type.func' [Type.func' [a; b] c; b; a] c in
      let act_tp = Type.simplify @@ to_type tm in
      assert_equal tm exp_tp act_tp ) ;

  ] ) ;

  ( "Church Booleans", [

    ( "false", fun to_type _ ->
      let tm = Term.abs' ["a"; "b"] (Term.var "a") in
      let exp_tp = Type.func' [a; b] a in
      let act_tp = Type.simplify @@ to_type tm in
      assert_equal tm exp_tp act_tp ) ;

    ( "false", fun to_type _ ->
      let tm = Term.abs' ["a"; "b"] (Term.var "b") in
      let exp_tp = Type.func' [a; b] b in
      let act_tp = Type.simplify @@ to_type tm in
      assert_equal tm exp_tp act_tp ) ;

    ( "if", fun to_type _ ->
      let tm =
        Term.abs'
          ["p"; "a"; "b"]
          (Term.app' (Term.var "p") [Term.var "a"; Term.var "b"])
      in
      let exp_tp = Type.func' [Type.func' [a; b] c; a; b] c in
      let act_tp = Type.simplify @@ to_type tm in
      assert_equal tm exp_tp act_tp ) ;

  ] ) ;

  ( "Church Naturals", [

    ( "zero", fun to_type _ ->
      let tm = Term.abs' ["f"; "x"] (Term.var "x") in
      let exp_tp = Type.func' [a; b] b in
      let act_tp = Type.simplify @@ to_type tm in
      assert_equal tm exp_tp act_tp ) ;

    ( "one", fun to_type _ ->
      let tm =
        Term.abs' ["f"; "x"] (Term.app (Term.var "f") (Term.var "x"))
      in
      let exp_tp = Type.func' [Type.func a b; a] b in
      let act_tp = Type.simplify @@ to_type tm in
      assert_equal tm exp_tp act_tp ) ;

    ( "two", fun to_type _ ->
      let tm =
        Term.abs'
          ["f"; "x"]
          (Term.app
            (Term.var "f")
            (Term.app (Term.var "f") (Term.var "x")))
      in
      let exp_tp = Type.func' [Type.func a a; a] a in
      let act_tp = Type.simplify @@ to_type tm in
      assert_equal tm exp_tp act_tp ) ;

    ( "three", fun to_type _ ->
      let tm =
        Term.abs'
          ["f"; "x"]
          (Term.app
            (Term.var "f")
            (Term.app
              (Term.var "f")
              (Term.app
                (Term.var "f")
                (Term.var "x"))))
      in
      let exp_tp = Type.func' [Type.func a a; a] a in
      let act_tp = Type.simplify @@ to_type tm in
      assert_equal tm exp_tp act_tp ) ;

    ( "succ", fun to_type _ ->
      let tm =
        Term.abs'
          ["n"; "f"; "x"]
          (Term.app
            (Term.var "f")
            (Term.app' (Term.var "n") [Term.var "f"; Term.var "x"]))
      in
      let exp_tp =
        Type.func'
          [Type.func' [Type.func a b; c] a; Type.func a b; c]
          b
      in
      let act_tp = Type.simplify @@ to_type tm in
      assert_equal tm exp_tp act_tp ) ;

    ( "pred", fun to_type _ ->
      let tm =
        Term.abs'
          ["n"; "f"; "x"]
          (Term.app'
            (Term.var "n")
            [
              Term.abs'
                ["g"; "h"]
                (Term.app
                  (Term.var "h")
                  (Term.app (Term.var "g") (Term.var "f"))) ;
              Term.abs "u" (Term.var "x") ;
              Term.abs "u" (Term.var "u") ;
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
      let act_tp = Type.simplify @@ to_type tm in
      assert_equal tm exp_tp act_tp ) ;

    ( "plus", fun to_type _ ->
      let tm =
        Term.abs'
          ["m"; "n"; "f"; "x"]
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
      let act_tp = Type.simplify @@ to_type tm in
      assert_equal tm exp_tp act_tp ) ;

    ( "minus", fun to_type _ ->
      let tm =
        Term.abs'
          ["m"; "n"]
          (Term.app'
            (Term.var "n")
            [
              Term.abs'
                ["n"; "f"; "x"]
                (Term.app'
                  (Term.var "n")
                  [
                    Term.abs'
                      ["g"; "h"]
                      (Term.app
                        (Term.var "h")
                        (Term.app (Term.var "g") (Term.var "f"))) ;
                    Term.abs "u" (Term.var "x") ;
                    Term.abs "u" (Term.var "u") ;
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
      let act_tp = Type.simplify @@ to_type tm in
      assert_equal tm exp_tp act_tp ) ;

    ( "mult", fun to_type _ ->
      let tm =
        Term.abs'
          ["m"; "n"; "f"]
          (Term.app
            (Term.var "m")
            (Term.app (Term.var "n") (Term.var "f")))
      in
      let exp_tp = Type.func' [Type.func a b; Type.func c a; c] b in
      let act_tp = Type.simplify @@ to_type tm in
      assert_equal tm exp_tp act_tp ) ;

    ( "exp", fun to_type _ ->
      let tm =
        Term.abs' ["m"; "n"] (Term.app (Term.var "n") (Term.var "m"))
      in
      let exp_tp = Type.func' [a; Type.func a b] b in
      let act_tp = Type.simplify @@ to_type tm in
      assert_equal tm exp_tp act_tp ) ;

  ] ) ;

]

let make_test_suite tests =
  let mapper to_type (name, tests) =
    name >:::
      List.map
        (fun (name, fn) -> Id.reset (); name >:: fn to_type)
        tests
  in
  "Tests" >::: [
    "Hindley-Milner" >:::
      List.map (mapper (fun tm -> Term.to_type_hm tm)) tests ;
    "Pottier-Remy" >:::
      List.map (mapper (fun tm -> Term.to_type_pr tm)) tests ;
  ]

let _ = run_test_tt_main (make_test_suite tests)
