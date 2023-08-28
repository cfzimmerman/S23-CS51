(*
                         CS 51 Problem Set 1
                Core Functional Programming -- Testing
*)

open Ps1

(* The Absbook module contains simple functions for unit testing:
   `unit_test` and `unit_test_within`. *)
open CS51Utils
open Absbook

let nonincreasing_test () =
  unit_test (nonincreasing []) "\n🔖 nonincreasing: empty";
  unit_test (nonincreasing [ 7 ]) "🔖 nonincreasing: single";
  unit_test (nonincreasing [ 4; 4; 4 ]) "🔖 nonincreasing: repeat";
  unit_test (not (nonincreasing [ 2; 1; 2 ])) "🔖 nonincreasing: inc at start";
  unit_test (nonincreasing [ 2; 2; 1 ]) "🔖 nonincreasing: dups";
  unit_test
    (nonincreasing [ 9; 8; 7; 6; 5; 5; 5; 4; 4; ~-2 ])
    "🔖 nonincreasing: long with neg";
  unit_test
    (not (nonincreasing [ 9; 8; 7; 6; 7; 5; 5; 5; 5; 4; 3 ]))
    "🔖 nonincreasing: long inc at mid"

let merge_test () =
  unit_test (merge [] [] = []) "\n🔖 merge: both empty lists";
  unit_test (merge [ 1 ] [] = [ 1 ]) "🔖 merge: left empty list";
  unit_test (merge [] [ 2 ] = [ 2 ]) "🔖 merge: right empty list";
  unit_test (merge [ 3 ] [ 3 ] = [ 3; 3 ]) "🔖 merge: identical single lists";
  unit_test
    (merge [ 4; 6; 8 ] [ 5; 7; 9 ] = [ 4; 5; 6; 7; 8; 9 ])
    "🔖 merge: fitted lists";
  unit_test
    (merge [ 13; 14; 15 ] [ 10; 11; 12 ] = [ 10; 11; 12; 13; 14; 15 ])
    "🔖 merge: lopsided lists";
  unit_test
    (merge [ ~-18; ~-17; ~-16 ] [ ~-17; ~-17; ~-17 ]
    = [ ~-18; ~-17; ~-17; ~-17; ~-17; ~-16 ])
    "🔖 merge: lopsided negative lists"

let zip_test () =
  unit_test (unzip [] = ([], [])) "\n🔖 zip: empty";
  unit_test (unzip [ (true, false) ] = ([ true ], [ false ])) "🔖 zip: single";
  unit_test
    (unzip [ (true, false); (false, true); (true, false) ]
    = ([ true; false; true ], [ false; true; false ]))
    "🔖 zip: many"

let from_run_length_test () =
  unit_test (from_run_length [] = []) "\n🔖 from_run_length: empty";
  unit_test
    (from_run_length [ (3, 'a') ] = [ 'a'; 'a'; 'a' ])
    "🔖 from_run_length: mono letter";
  unit_test
    (from_run_length [ (1, 'a'); (2, 'b') ] = [ 'a'; 'b'; 'b' ])
    "🔖 from_run_length: simple sequence";
  unit_test
    (from_run_length [ (5, 'a'); (3, 'b'); (1, 'a'); (4, 'd') ]
    = [ 'a'; 'a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'a'; 'd'; 'd'; 'd'; 'd' ])
    "🔖 from_run_length: long sequence"

let to_run_length_test () =
  unit_test (to_run_length [] = []) "\n🔖 to_run_length: empty";
  unit_test
    (to_run_length [ 'a'; 'a'; 'a' ] = [ (3, 'a') ])
    "🔖 to_run_length: mono letter";
  unit_test
    (to_run_length [ 'a'; 'b'; 'b' ] = [ (1, 'a'); (2, 'b') ])
    "🔖 to_run_length: simple sequence";
  unit_test
    (to_run_length
       [ 'a'; 'a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'a'; 'd'; 'd'; 'd'; 'd' ]
    = [ (5, 'a'); (3, 'b'); (1, 'a'); (4, 'd') ])
    "🔖 to_run_length: long sequence"

let extract_entry_test () =
  unit_test
    (extract_entry (cCOOPERATE, cCOOPERATE) [] = (404, 404))
    "\n🔖 extract_entry: empty payoff_matrix";
  unit_test
    (extract_entry (cCOOPERATE, cCOOPERATE)
       [ ((cCOOPERATE, cCOOPERATE), (3, 3)) ]
    = (3, 3))
    "🔖 extract_entry: simple payoff_matrix";
  unit_test
    (extract_entry (cCOOPERATE, cCOOPERATE)
       [ ((cCOOPERATE, cDEFECT), (~-2, 5)) ]
    = (404, 404))
    "🔖 extract_entry: simple not found";
  unit_test
    (extract_entry (cDEFECT, cCOOPERATE) test_payoff_matrix = (5, ~-2))
    "🔖 extract_entry: complex payoff_matrix"

let count_defections_test () =
  unit_test (count_defections [] = (0, 0)) "\n🔖 count_defections: empty history";
  unit_test
    (count_defections [ (cDEFECT, cDEFECT) ] = (1, 1))
    "🔖 count_defections: single defections";
  unit_test
    (count_defections [ (cCOOPERATE, cCOOPERATE) ] = (0, 0))
    "🔖 count_defections: no defections";
  unit_test
    (count_defections
       [
         (cDEFECT, cCOOPERATE);
         (cDEFECT, cCOOPERATE);
         (cDEFECT, cDEFECT);
         (cCOOPERATE, cCOOPERATE);
       ]
    = (3, 1))
    "🔖 count_defections: complex defections"

let count_cooperations_test () =
  unit_test
    (count_cooperations [] = (0, 0))
    "\n🔖 count_cooperations: empty history";
  unit_test
    (count_cooperations [ (cDEFECT, cDEFECT) ] = (0, 0))
    "🔖 count_cooperations: no cooperations";
  unit_test
    (count_cooperations [ (cCOOPERATE, cCOOPERATE) ] = (1, 1))
    "🔖 count_cooperations: single cooperations";
  unit_test
    (count_cooperations
       [
         (cDEFECT, cCOOPERATE);
         (cDEFECT, cCOOPERATE);
         (cDEFECT, cDEFECT);
         (cCOOPERATE, cCOOPERATE);
       ]
    = (1, 3))
    "🔖 count_cooperations: complex cooperations"

let balanced_test () =
  unit_test (balanced [] = cCOOPERATE) "\n🔖 balanced: empty history";
  unit_test
    (balanced [ (cDEFECT, cCOOPERATE); (cCOOPERATE, cCOOPERATE) ] = cCOOPERATE)
    "🔖 balanced: last defected";
  unit_test
    (balanced [ (cCOOPERATE, cCOOPERATE); (cCOOPERATE, cCOOPERATE) ] = cDEFECT)
    "🔖 balanced: last cooperated";
  unit_test
    (balanced
       [
         (cCOOPERATE, cDEFECT);
         (cCOOPERATE, cCOOPERATE);
         (cCOOPERATE, cDEFECT);
         (cDEFECT, cCOOPERATE);
         (cCOOPERATE, cDEFECT);
       ]
    = cDEFECT)
    "🔖 balanced: long history"

let egalitarian_test () =
  unit_test (egalitarian [] = cCOOPERATE) "\n🔖 egalitarian: empty history";
  unit_test
    (egalitarian [ (cDEFECT, cCOOPERATE); (cCOOPERATE, cCOOPERATE) ]
    = cCOOPERATE)
    "🔖 egalitarian: last defected";
  unit_test
    (egalitarian [ (cCOOPERATE, cDEFECT); (cCOOPERATE, cCOOPERATE) ] = cDEFECT)
    "🔖 egalitarian: last cooperated";
  unit_test
    (egalitarian
       [
         (cCOOPERATE, cDEFECT);
         (cCOOPERATE, cCOOPERATE);
         (cCOOPERATE, cDEFECT);
         (cDEFECT, cCOOPERATE);
         (cCOOPERATE, cDEFECT);
       ]
    = cDEFECT)
    "🔖 egalitarian: long history"

let tit_for_tat_test () =
  unit_test (tit_for_tat [] = cCOOPERATE) "\n🔖 tit_for_tat: empty history";
  unit_test
    (tit_for_tat [ (cCOOPERATE, cCOOPERATE); (cCOOPERATE, cCOOPERATE) ]
    = cCOOPERATE)
    "🔖 tit_for_tat: opponent last cooperated";
  unit_test
    (tit_for_tat [ (cDEFECT, cDEFECT); (cCOOPERATE, cDEFECT) ] = cDEFECT)
    "🔖 tit_for_tat: opponent last defected";
  unit_test
    (tit_for_tat
       [
         (cCOOPERATE, cDEFECT);
         (cCOOPERATE, cCOOPERATE);
         (cDEFECT, cCOOPERATE);
         (cCOOPERATE, cDEFECT);
       ]
    = cDEFECT)
    "🔖 tit_for_tat: long history"

let my_strategy_test () =
  unit_test (my_strategy [] = cCOOPERATE) "\n🔖 my_strategy: empty history";
  unit_test
    (my_strategy [ (cCOOPERATE, cCOOPERATE); (cCOOPERATE, cCOOPERATE) ]
    = cCOOPERATE)
    "🔖 my_strategy: all cooperating";
  unit_test
    (my_strategy [ (cCOOPERATE, cDEFECT) ] = cDEFECT)
    "🔖 my_strategy: single betrayal";
  unit_test
    (decide_move 1 0
       [
         (cDEFECT, cCOOPERATE); (cCOOPERATE, cDEFECT); (cCOOPERATE, cCOOPERATE);
       ]
    = cDEFECT)
    "🔖 my_strategy: remember betrayal";
  unit_test
    (decide_move 1 0
       [
         (cDEFECT, cCOOPERATE);
         (cDEFECT, cCOOPERATE);
         (cCOOPERATE, cDEFECT);
         (cCOOPERATE, cCOOPERATE);
       ]
    = cCOOPERATE)
    "🔖 my_strategy: forget betrayal"

let swap_actions_test () =
  unit_test (swap_actions [] = []) "\n🔖 swap_actions: empty history";
  unit_test
    (swap_actions [ (cCOOPERATE, cDEFECT) ] = [ (cDEFECT, cCOOPERATE) ])
    "🔖 swap_actions: simple swap";
  unit_test
    (swap_actions [ (cDEFECT, cCOOPERATE); (cCOOPERATE, cCOOPERATE) ]
    = [ (cCOOPERATE, cDEFECT); (cCOOPERATE, cCOOPERATE) ])
    "🔖 swap_actions: complex swap"

let calculate_payoff_test () =
  unit_test
    (calculate_payoff test_payoff_matrix [] = (0, 0))
    "\n🔖 calculate_payoff: empty history";
  unit_test
    (calculate_payoff test_payoff_matrix
       [ (cCOOPERATE, cCOOPERATE); (cCOOPERATE, cDEFECT) ]
    = (1, 8))
    "🔖 calculate payoff: simple history";
  unit_test
    (calculate_payoff test_payoff_matrix
       [
         (cCOOPERATE, cCOOPERATE);
         (cDEFECT, cDEFECT);
         (cDEFECT, cCOOPERATE);
         (cCOOPERATE, cDEFECT);
       ]
    = (6, 6))
    "🔖 calculate payoff: complex history"

let test_all () =
  nonincreasing_test ();
  merge_test ();
  zip_test ();
  from_run_length_test ();
  to_run_length_test ();
  extract_entry_test ();
  count_defections_test ();
  count_cooperations_test ();
  balanced_test ();
  egalitarian_test ();
  tit_for_tat_test ();
  my_strategy_test ();
  swap_actions_test ();
  calculate_payoff_test ()

let _ = test_all ()
;;