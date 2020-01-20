module Vec = Vector
open OUnit
open QCheck

let of_list l = List.fold_left (fun v x -> Vec.push_back x v) Vec.empty l

let count = 1000

let suite =
  "Vector tests"
  >::: List.map
         (fun test -> QCheck_ounit.to_ounit_test test)
         [ Test.make ~name:"0 = size empty" ~count unit (fun () ->
               0 = Vec.size Vec.empty);
           Test.make
             ~name:"length l = size (of_list l)"
             ~count
             (list int)
             (fun l -> List.length l = Vec.size @@ of_list l);
           Test.make
             ~name:"x = get i (set i x v)"
             ~count
             (triple small_nat int @@ list int)
             (fun (i, x, l) ->
               assume (List.length l > i);
               let v = of_list l in
               x = Vec.get i @@ Vec.set i x v);
           Test.make
             ~name:"get i v = get i (set j x v) | i <> j"
             ~count
             (quad small_nat small_nat int (list int))
             (fun (i, j, x, l) ->
               assume (List.length l > max i j);
               assume (i <> j);
               let v = of_list l in
               Vec.get i v = Vec.get i @@ Vec.set j x v);
           Test.make
             ~name:"x = peek (push x v)"
             ~count
             (pair int (list int))
             (fun (x, l) ->
               let v = of_list l in
               x = Vec.peek_back @@ Vec.push_back x v);
           Test.make
             ~name:"get i v = get i (push x v)"
             ~count
             (triple small_nat int @@ list int)
             (fun (i, x, l) ->
               assume (List.length l > i);
               let v = of_list l in
               Vec.get i v = Vec.get i @@ Vec.push_back x v);
           Test.make
             ~name:"size v = size (pop (push x v))"
             ~count
             (pair int (list int))
             (fun (x, l) ->
               let v = of_list l in
               Vec.size v = Vec.size @@ Vec.pop_back @@ Vec.push_back x v) ]

let _ = run_test_tt_main suite
