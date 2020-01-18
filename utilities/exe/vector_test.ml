open OUnit
open QCheck
open Vector

let count = 1000

let suite =
  "Vector tests"
  >::: List.map
         (fun test -> QCheck_ounit.to_ounit_test test)
         [ Test.make ~name:"[] = to_list empty" ~count unit (fun () ->
               [] = to_list empty);
           Test.make
             ~name:"length l = size (of_list l)"
             ~count
             (list int)
             (fun l -> List.length l = size @@ of_list l);
           Test.make ~name:"l = to_list (of_list v)" ~count (list int) (fun l ->
               l = to_list @@ of_list l);
           Test.make
             ~name:"x = get i (set i x v)"
             ~count
             (triple small_nat int @@ list int)
             (fun (i, x, l) ->
               let v = of_list l in
               assume (size v > i);
               x = get i @@ set i x v);
           Test.make
             ~name:"get i v = get i (set j x v) | i <> j"
             ~count
             (quad small_nat small_nat int (list int))
             (fun (i, j, x, l) ->
               let v = of_list l in
               assume (size v > max i j);
               assume (i <> j);
               get i v = get i @@ set j x v);
           Test.make
             ~name:"x = peek (push x v)"
             ~count
             (pair int (list int))
             (fun (x, l) ->
               let v = of_list l in
               x = peek_back @@ push_back x v);
           Test.make
             ~name:"get i v = get i (push x v)"
             ~count
             (triple small_nat int @@ list int)
             (fun (i, x, l) ->
               let v = of_list l in
               assume (size v > i);
               get i v = get i @@ push_back x v);
           Test.make
             ~name:"size v = size (pop (push x v))"
             ~count
             (pair int (list int))
             (fun (x, l) ->
               let v = of_list l in
               size v = size @@ pop_back @@ push_back x v) ]

let _ = run_test_tt_main suite
