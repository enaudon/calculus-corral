module DS = Disjoint_set
module Opt = Option
module Vec = Vector
open OUnit
open QCheck

let of_singleton_list es =
  List.fold_left
    (fun (ds, es) x ->
      let ds, elt = DS.singleton x ds in
      (ds, Vec.push_back elt es))
    (DS.empty, Vec.empty)
    es

let of_merge_list merges es =
  let ds, es = of_singleton_list es in
  ( merges
    |> List.map (fun (i1, i2) -> (Vec.get i1 es, Vec.get i2 es))
    |> List.fold_left (fun ds (e1, e2) -> DS.merge e1 e2 ds) ds,
    es )

let count = 1000

let suite =
  "Disjoint set tests"
  >::: List.map
         (fun test -> QCheck_ounit.to_ounit_test test)
         [ Test.make
             ~name:"find elt ds = find elt (singleton x ds)"
             ~count
             (triple small_nat int @@ list int)
             (fun (i, x, l) ->
               assume (List.length l > i);
               let ds, es = of_singleton_list l in
               let elt = Vec.get i es in
               let _, e1 = DS.find elt ds in
               let _, e2 = DS.find elt (fst @@ DS.singleton x ds) in
               e1 = e2);
           Test.make
             ~name:"merge elt1 elt2 ds; find elt1 ds = find elt2 ds"
             ~count
             ( quad
                 small_nat
                 small_nat
                 (list small_int)
                 (list @@ pair small_nat small_nat)
             |> set_shrink Shrink.nil )
             (fun (i, j, l, merges) ->
               let len = List.length l in
               assume (List.for_all (fun (i, j) -> len > max i j) merges);
               assume (len > max i j);
               let ds, es = of_merge_list merges l in
               let elt1 = Vec.get i es in
               let elt2 = Vec.get j es in
               let ds = DS.merge elt1 elt2 ds in
               snd @@ DS.find elt1 ds = snd @@ DS.find elt2 ds) ]

let _ = run_test_tt_main suite
