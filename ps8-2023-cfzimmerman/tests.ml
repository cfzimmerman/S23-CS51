open Passert
open Counter

let test_counter () =
  let random_num = 10 in
  let expect = passert ~suite:"counter" in
  let ctr = new counter in
  expect (ctr#count = 0) "initializes to zero";
  expect
    (ctr#set random_num;
     ctr#count = random_num)
    "set";
  expect
    (ctr#bump;
     ctr#count = random_num + 1)
    "bump";
  expect
    (ctr#debump;
     ctr#count = random_num)
    "debump"

let run_tests () = test_counter ()

let _ = run_tests ()
