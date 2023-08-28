(*
                          CS 51 Problem Set 2
             Higher Order Functional Programming -- Testing
*)

open Mapfold
open CS51Utils
open Absbook

let negate_all_test () =
  unit_test (negate_all [] = []) "\n🔖 negate_all: empty list";
  unit_test
    (negate_all [ 4; 5; 6; 7; 8 ] = [ ~-4; ~-5; ~-6; ~-7; ~-8 ])
    "🔖 negate_all: all positive";
  unit_test
    (negate_all [ ~-4; ~-5; ~-6; ~-7; ~-8 ] = [ 4; 5; 6; 7; 8 ])
    "🔖 negate_all: all negative";
  unit_test
    (negate_all [ 1; ~-2; 0 ] = [ ~-1; 2; 0 ])
    "🔖 negate_all: mixed list"

let sum_test () =
  unit_test (sum [] = 0) "\n🔖 sum: empty list";
  unit_test (sum [ 1; 2; 3; 4 ] = 10) "🔖 sum: all positive";
  unit_test (sum [ ~-1; ~-2; ~-3; ~-4 ] = ~-10) "🔖 sum: all negative";
  unit_test (sum [ 1; ~-2; 3; ~-4 ] = ~-2) "🔖 sum: mixed"
      
let sum_rows_test () =
  unit_test (sum_rows [] = []) "\n🔖 sum_rows: empty matrix";
  unit_test (sum_rows [ [] ] = [ 0 ]) "🔖 sum_rows: empty x empty matrix";
  unit_test
    (sum_rows [ [ 1; 2 ]; [ 3; 4 ] ] = [ 3; 7 ])
    "🔖 sum_rows: all positive";
  unit_test
    (sum_rows [ [ 1; 2 ]; []; [ 5; 6 ] ] = [ 3; 0; 11 ])
    "🔖 sum_rows: empty sublist";
  unit_test
    (sum_rows [ [ ~-1; 1 ]; [ 5; ~-11 ] ] = [ 0; ~-6 ])
    "🔖 sum_rows: mixed list"

let filter_odd_test () =
  unit_test (filter_odd [] = []) "\n🔖 filter_odd: empty list";
  unit_test (filter_odd [ 2; 4; 6; 8 ] = []) "🔖 filter_odd: all evens";
  unit_test
    (filter_odd [ ~-1; 3; ~-7; 11 ] = [ ~-1; 3; ~-7; 11 ])
    "🔖 filter_odd: all odds";
  unit_test (filter_odd [ ~-2; ~-3; 6; ~-8 ] = [ ~-3 ]) "🔖 filter_odd: mixed"

let num_occurs_test () =
  unit_test (num_occurs 1 [] = 0) "\n🔖 num_occurs: empty list";
  unit_test (num_occurs 4 [ 1; 3; 4; 5; 4 ] = 2) "🔖 num_occurs: all positive";
  unit_test
    (num_occurs 4 [ ~-1; 3; ~-4; 5; 4 ] = 1)
    "🔖 num_occurs: mixed values"

let super_sum_test () =
  unit_test (super_sum [] = 0) "\n🔖 super_sum: empty matrix";
  unit_test
    (super_sum [ [ 1; 2; 3 ]; []; [ 5 ] ] = 11)
    "🔖 super_sum: empty sublist";
  unit_test (super_sum [ []; []; [] ] = 0) "🔖 super_sum: all empty sublists";
  unit_test
    (super_sum [ [ ~-1; ~-2; 3; ~-4 ]; [ 5 ]; [ ~-25; 5 ] ] = ~-19)
    "🔖 super_sum: mixed sublists"

let filter_range_test () =
  unit_test ((filter_range []) (3, 5) = []) "\n🔖 super_sum: empty list";
  unit_test
    ((filter_range [ 1; 3; 4; 5; 2 ]) (1, 3) = [ 1; 3; 2 ])
    "🔖 super_sum: simple case";
  unit_test
    ((filter_range [ 1; 3; 4; 5; 2 ]) (3, 2) = [])
    "🔖 super_sum: empty range";
  unit_test
    ((filter_range [ 1; 2; ~-7; ~-3; 4; ~-2 ]) (~-5, ~-2) = [ ~-3; ~-2 ])
    "🔖 super_sum: negative range"

let floats_of_ints_test () =
  unit_test (floats_of_ints [] = []) "\n🔖 floats_of_ints: empty list";
  unit_test
    (floats_of_ints [ 1; 2; 3 ] = [ 1.; 2.; 3. ])
    "🔖 floats_of_ints: all positives";
  unit_test
    (floats_of_ints [ 1; ~-2; 3; ~-54 ] = [ 1.; -2.; 3.; -54. ])
    "🔖 floats_of_ints: mixed list"

let log10s_test () =
  unit_test (log10s [] = []) "\n🔖 log10s: empty list";
  unit_test
    (log10s [ 1.0; 10.0; -10.0 ] = [ Some 0.; Some 1.; None ])
    "🔖 log10s: mixed list"

let deoptionalize_test () =
  unit_test (deoptionalize [] = []) "\n🔖 deoptionalize: empty list";
  unit_test
    (deoptionalize [ Some 3; None; Some ~-5; Some 10 ] = [ 3; ~-5; 10 ])
    "🔖 deoptionalize: int list";
  unit_test
    (deoptionalize [ Some 3.; None; Some (-5.); Some 10. ] = [ 3.; -5.; 10. ])
    "🔖 deoptionalize: float list";
  unit_test
    (deoptionalize [ None; Some "Hey there 😉"; None; None ] = [ "Hey there 😉" ])
    "🔖 deoptionalize: string list"

let some_sum_test () =
  unit_test (some_sum [] = 0) "\n🔖 some_sum: empty list";
  unit_test
    (some_sum [ Some 3; None; Some ~-5; Some 10 ] = 8)
    "🔖 some_sum: basic list";
  unit_test (some_sum [ None; None; None ] = 0) "🔖 some_sum: none list"

let mult_odds_test () =
  unit_test (mult_odds [] = 0) "\n🔖 mult_odds: empty list";
  unit_test (mult_odds [ 1; 3; 0; 2; ~-5 ] = ~-15) "🔖 mult_odds: mixed list";
  unit_test (mult_odds [ 2; 4; 6; 8 ] = 0) "🔖 mult_odds: all evens"

let concat_test () =
  unit_test (concat [] = []) "\n🔖 concat: empty list";
  unit_test (concat [ []; []; [] ] = []) "🔖 concat: empty matrix";
  unit_test
    (concat [ [ 1; 2 ]; []; [ 3; ~-4; 0 ]; [ 6 ] ] = [ 1; 2; 3; ~-4; 0; 6 ])
    "🔖 concat: mixed int list";
  unit_test
    (concat [ []; [ 4.; -2. ]; [ 32. ] ] = [ 4.; -2.; 32. ])
    "🔖 concat: mixed float list";
  unit_test
    (concat [ [ "Hey"; "there"; "autograder!" ]; [ "🤘" ]; [] ]
    = [ "Hey"; "there"; "autograder!"; "🤘" ])
    "🔖 concat: string list"

let filter_by_year_test () =
  unit_test (filter_by_year [] 2022 = []) "\n🔖 filter_by_year: empty list";
  unit_test
    (filter_by_year
       [ ("Joe", 2010); ("Bob", 2010); ("Tom", 2013); ("Kim", 2010) ]
       2010
    = [ "Joe"; "Bob"; "Kim" ])
    "🔖 filter_by_year: standard list";
  unit_test
    (filter_by_year [ ("Joe", 2010); ("Bob", 2010); ("Tom", 2013) ] 2023 = [])
    "🔖 filter_by_year: empty year"

let test () =
  negate_all_test ();
  sum_test ();
  sum_rows_test ();
  filter_odd_test ();
  num_occurs_test ();
  super_sum_test ();
  filter_range_test ();
  floats_of_ints_test ();
  log10s_test ();
  deoptionalize_test ();
  some_sum_test ();
  mult_odds_test ();
  concat_test ();
  filter_by_year_test ()
;;

test ()

;;