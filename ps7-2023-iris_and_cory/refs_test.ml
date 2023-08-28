(*
                         CS 51 Problem Set 7
                       Refs, Streams, and Music
                             Refs Testing
 *)

open Passert
open Refs

(* Main testable lists *)
(* Some tests depend on acyclic_list being a non-cyclic copy of cyclic-list, so they should mirror each other. *)
let acyclic_list =
  ref
    (Cons
       ( 1,
         ref
           (Cons
              ( 2,
                ref
                  (Cons
                     ( 3,
                       ref
                         (Cons
                            ( 4,
                              ref
                                (Cons
                                   (6, ref (Cons (7, ref (Cons (8, ref Nil))))))
                            )) )) )) ))

let listend = ref Nil

let cyclic_list =
  ref
    (Cons
       ( 1,
         ref
           (Cons
              ( 2,
                ref
                  (Cons
                     ( 3,
                       ref
                         (Cons
                            ( 4,
                              ref
                                (Cons
                                   (6, ref (Cons (7, ref (Cons (8, listend))))))
                            )) )) )) ))

let _ = listend := !cyclic_list

(* Mono lists verifying equality is physical and not structural  *)
let acyclic_mono =
  ref
    (Cons
       ( 'm',
         ref
           (Cons
              ( 'm',
                ref
                  (Cons
                     ( 'm',
                       ref
                         (Cons ('m', ref (Cons ('m', ref (Cons ('m', ref Nil))))))
                     )) )) ))

let listend = ref Nil

let cyclic_mono =
  ref
    (Cons
       ( 'm',
         ref
           (Cons
              ( 'm',
                ref
                  (Cons
                     ( 'm',
                       ref
                         (Cons ('m', ref (Cons ('m', ref (Cons ('m', listend))))))
                     )) )) ))

let _ = listend := !cyclic_mono

let test_has_cycle () =
  let expect = passert ~suite:"has_cycle" in
  expect (not (has_cycle (ref Nil))) "empty list";
  expect (not (has_cycle acyclic_list)) "no cycle present";
  expect (has_cycle cyclic_list) "detected cycle";
  expect (not (has_cycle acyclic_mono)) "no cycle, mono";
  expect (has_cycle cyclic_mono) "detected cycle, mono"

let rec print_char_lst (lst : char mlist) : unit =
  match !lst with
  | Nil -> Printf.printf "Nil\n"
  | Cons (hd, tl) ->
      Printf.printf "%c, " hd;
      print_char_lst tl

let test_flatten () =
  (* these tests intentionally call "ref !" so we don't modify the template lists. *)
  let expect = passert ~suite:"flatten" in
  let mut_nil = ref Nil in
  flatten mut_nil;
  expect (!mut_nil = Nil) "empty list";
  let mut_acyc_lst = ref !acyclic_list in
  flatten mut_acyc_lst;
  expect (!mut_acyc_lst = !acyclic_list) "no cycle in ints";
  let mut_acyc_mono = ref !acyclic_mono in
  flatten mut_acyc_mono;
  expect (!mut_acyc_mono = !acyclic_mono) "no cycle in mono chars";
  let mut_cyc_lst = ref !cyclic_list in
  flatten mut_cyc_lst;
  expect (!mut_cyc_lst = !acyclic_list) "broke cycle";
  let mut_cyc_mono = ref !cyclic_mono in
  flatten mut_cyc_mono;
  expect (!mut_cyc_mono = !cyclic_mono) "broke mono cycle"

let test_mlength () =
  let expect = passert ~suite:"mlength" in
  expect (mlength (ref Nil) = 0) "empty list";
  expect (mlength acyclic_list = 7) "acyclic ints";
  expect (mlength acyclic_mono = 6) "acyclic mono chars";
  expect (mlength cyclic_list = 7) "cyclic ints";
  expect (mlength cyclic_mono = 6) "cyclic mono chars"

let tests () =
  test_has_cycle ();
  test_flatten ();
  test_mlength ()

let _ = tests ()
