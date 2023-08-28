(*
                          CS 51 Problem Set 4
                  A Language for Symbolic Mathematics
                                Testing
*)

open Expression
open ExpressionLibrary
open CS51Utils
open Absbook

let contains_var_test () =
  unit_test (contains_var (parse "x+3")) "\n🔖 contains_var: sum left";
  unit_test (not (contains_var (parse "2"))) "🔖 contains_var: number";
  unit_test (contains_var (parse "cos x")) "🔖 contains_var: cos x";
  unit_test
    (contains_var (parse "cos 3 - 5 * ~x"))
    "🔖 contains_var: complex expression"

let evaluate_test () =
  unit_test (evaluate (parse "x+3") 5.0 = 8.) "\n🔖 evaluate: simple addition";
  unit_test
    (evaluate (parse "~((x + 1)^(3 - 1))") 2.0 = -9.)
    "🔖 evaluate: negative, power";
  unit_test
    (evaluate (parse "(x + 5 + (cos 0)) / (4 + (ln 1))") 2.0 = 2.)
    "🔖 evaluate: ln, cos, divide"

let derivative_test () =
  unit_test
    (evaluate (derivative (parse "x")) 5. = 1.)
    "\n🔖 derivative: d/dx variable";
  unit_test
    (evaluate (derivative (parse "sin (3 * x)")) 0. = 3.)
    "🔖 derivative: d/dx sin";
  unit_test
    (evaluate (derivative (parse "ln(x^2)")) 4. = 0.5)
    "🔖 derivative: d/dx ln";
  unit_test
    (evaluate (derivative (parse "(x^2) / (3 + (2 * x))")) 6. = 0.48)
    "🔖 derivative: d/dx division";
  unit_test
    (evaluate (derivative (parse "x^4")) 2. = 32.)
    "🔖 derivative: d/dx power"

let tolerance = 0.00000001

let _fz_test (fz : float option) (expected : float) : bool =
  match fz with
  | None -> false
  | Some value -> Float.abs value -. Float.abs expected <= tolerance

let find_zero_test () =
  unit_test
    (_fz_test (find_zero (parse "3 * x - 1") 0. tolerance 100) (1. /. 3.))
    "\n🔖 find_zero: 3x - 1";
  unit_test
    (_fz_test
       (find_zero (parse "(2*x + 1) * (~(x^3) - 5)") 0. tolerance 100)
       (-0.5))
    "🔖 find_zero: polynomial";
  unit_test
    (_fz_test (find_zero (parse "ln(x)") 2. tolerance 100) 1.)
    "🔖 find_zero: ln";
  unit_test
    (_fz_test
       (find_zero (parse "cos(3 * x)") 1. tolerance 100)
       (-1. *. (Float.pi *. (4. -. 1.)) /. 6.))
    "🔖 find_zero: cos";
  unit_test
    (_fz_test (find_zero (parse "(x + 4)^2") 0. tolerance 100) (-4.))
    "🔖 find_zero: power"

(* Start by making more tests tomorrow :) *)

let test () =
  contains_var_test ();
  evaluate_test ();
  derivative_test ();
  find_zero_test ()
;;

test ()

;;