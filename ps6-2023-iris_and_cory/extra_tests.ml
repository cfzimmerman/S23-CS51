open Passert
open Collections

(*
   
This file is used for more flexible, isolated, and readable testing than was 
feasible using the already-crowded tests.ml file. Currently, it's only used 
to test our stack-based queue implementation.

*)

module IntQueueStack = MakeQueueStack (struct
  type t = int
end)

let test_queue_stack () : unit =
  print_endline "\nTest queue_stack: ";
  let expect = passert ~suite:"queue_stack" in
  let open IntQueueStack in
  (* empty, is_empty *)
  expect (is_empty empty) "is_empty when empty";
  expect (length empty = 0) "empty queue has length 0";
  (* add, length *)
  expect
    (empty |> add 0 |> add 1 |> add 2 |> add 3 |> length = 4)
    "add elements";
  (* take *)
  let first_take = empty |> add 0 |> add 1 |> add 2 |> add 3 in
  let t1, c1 = take first_take in
  let t2, c2 = take c1 in
  let t3, c3 = take c2 in
  let t4, c4 = take c3 in
  let second_take = c4 |> add 4 |> add 5 in
  let t5, c5 = take second_take in
  let t6, c6 = take c5 in
  expect (t1 = 0) "first take 1";
  expect (t2 = 1) "first take 2";
  expect (t3 = 2) "first take 3";
  expect (t4 = 3) "first take 4";
  expect (is_empty c4) "took all";
  expect (length second_take = 2) "add two more";
  expect (t5 = 4) "then take 1";
  expect (t6 = 5) "then take 2";
  expect (is_empty c6) "took all again"
;;

test_queue_stack ()
