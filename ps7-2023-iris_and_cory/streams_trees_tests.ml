open Passert
open NativeLazyStreams
open Streamstrees
open SampleStreams

let test_aitken () =
  let expect = passert ~suite:"aitken" in
  expect (first 3 (aitken (to_float alt_signs)) = [ 0.; 0.; 0. ]) "ones"
(* This was hard to test because so many streams generate undefined results. *)

let test_node () =
  let expect = passert ~suite:"node" in
  expect (node onest = 1) "onest";
  expect (node (levels 5) = 5) "levels 5"

let test_children () =
  let expect = passert ~suite:"children" in
  expect (List.length (children onest) = 2) "onest"

let test_tmap () =
  let expect = passert ~suite:"tmap" in
  expect
    (first 3 (bfenumerate [ tmap (( + ) 4) onest ]) = [ 5; 5; 5 ])
    "onest plus 4";
  expect
    (first 7 (bfenumerate [ tmap (( + ) 0) (levels 0) ])
    = [ 0; 1; 1; 2; 2; 2; 2 ])
    "levels plus 0";
  expect
    (first 10 (bfenumerate [ tree_nats ]) = [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ])
    "tree_nats plus 0"

let test_tmap2 () =
  let expect = passert ~suite:"tmap2" in
  expect
    (first 3 (bfenumerate [ tmap2 (fun x y -> x + y) onest onest ])
    = [ 2; 2; 2 ])
    "onest plust onest";
  expect
    (first 7 (bfenumerate [ tmap2 (fun x y -> x + y) onest (levels 0) ])
    = [ 1; 2; 2; 3; 3; 3; 3 ])
    "ones plus levels";
  expect
    (first 5 (bfenumerate [ tmap2 (fun x y -> x * y) tree_nats tree_nats ])
    = [ 0; 1; 4; 9; 16 ])
    "nats squared"

(* bfenumerate and the other trees are implicitly tested by the other methods, so I'll leave them alone. *)

let test () =
  test_aitken ();
  test_node ();
  test_children ();
  test_tmap ();
  test_tmap2 ()

let _ = test ()
