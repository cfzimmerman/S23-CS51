(*
                              CS51 Lab 2
                     Basic Functional Programming
                         Some Testing Methods
 *)

(* Here, we introduce a few ways to test the functions you wrote in
   lab. One or more of these methods may be useful in the future as
   you will want to provide for unit-testing of your code for problem
   sets and the final project. As an example, we provide a few tests
   for the square_all function.
 *)

open Lab2 ;; (* for access to your lab2 solution *)
open CS51Utils.Absbook ;; (* for access to the unit_test function *)

(* Method 1: Boolean

   This method executes the tests right away, returning false on
   failure. Failure in these tests will only be detected by running
   this code in a REPL.  *)

let test_square_all_bool () : bool =
  square_all [] = []
  && square_all [1] = [1]
  && square_all [-1] = [1]
  && square_all [3; 4; 5] = [9; 16; 25]
  && square_all [4; -10; 12] = [16; 100; 144] ;;

(* Run the tests. Note that if the test function returns false, simply
compiling and running this file will not reveal the failure. *)

test_square_all_bool () ;;

(* Method 2: unit_test

   By making use of side effects, we can print an indicative message
   relating to each test. We will use the unit-test function
   provided in the Absbook module.  *)

let test_square_all () =
  unit_test (square_all [] = [])
            "square_all empty";
  unit_test (square_all [1] = [1])
            "square_all one";
  unit_test (square_all [-1] = [1])
            "square_all neg_one";
  unit_test (square_all [3; 4; 5] = [9; 16; 25])
            "square_all many_pos";
  unit_test (square_all [4; -10; 12] = [16; 100; 144])
            "square_all many_int";;

(* Now run the tests *)
let _ = test_square_all () ;;

(* To actually execute the test, you need to run this program. First 
   you'll need to compile the file with 

     % ocamlbuild -use-ocamlfind lab2_tests.byte

   Once you have a compiled file, you need to run the compiled code:

     % ./lab2_tests.byte

   We've provided a helpful makefile for you that will do both of
   these with the command

     % make tests *)
