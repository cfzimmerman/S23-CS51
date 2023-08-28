
(* The Absbook module contains simple functions for unit testing:
   `unit_test` and `unit_test_within`. *)
   open CS51Utils ;;
   open Absbook ;;



(* Values and Types 
.........................................*)


(* Exercise 0 

Write a function that add two integers in as many ways as possible 

*)




(* Exercise 0b

Write the list [2;4;7;11;13;15;19] in as many ways that you can think of using the :: (cons) operator (Stop at 5 or so) 

*)





(* 
Exercise 1 


Determine what the following expression evaluates to without using the OCaml interpreter 

*)

let exercise1 = 
    let y = 11 in
    (let x = 12 in x + 5) + y ;;


(* Exercise 2

What is the type of the following functions. Uncomment the function below and replace the ??? with the type. 

*)

(* let rec a : ??? -> 
    fun x -> 
		  match x with 
		  | [] -> (a tl)+ (if hd then 1 else 0)
		  | [] -> 3
*)



(* Exercise 3 

Fill in the type: uncomment the expression below

*)

(* 
let exercise3 : ??? = 
  let add (x : int) (y : int) : int =
    x + y
  in
  add 5 1 ;; 
*)


(* Excierse 4:  Fill in the type of the following exercise below *)

(* 
let exercise4 : ???  =  
    let add (x : int) (y : int) : int =
      x + y in
      add 2 ;; 
*)


(* Exercise 5 : Fill in the type of the following exercise  below  *)

(* 

    let exercise5 : ??? = fun x -> fun y -> 10 * x + y ;;  
    
*)



(* Exercise 6: Fill in the type of the following exercise  below *)

(* 

let exercise6 : ??? = 
  fun a b c ->
    if c then a *. float_of_int b
    else a ;; 
  
*)

(* 

Exercise 7 : Fill out the type of the following exercise below 
  
*)


(* 

let exercise7 = 
  let greet y = "Hello " ^ y in
      fun a -> fun b -> if a then b else greet b ;; 
      
*)



(* 

Recursion 

*)

(* Exercise  8: 

Define a recursive function remove_adjacents that removes returns a sublist of an int list such that adjacent elements are 

distinct. You can also think of this as a function that removes all duplicates of a list in sorted order. Do this without using the 

List Module. 

# remove_adjacents [3;1;2;4;3;3] ;;
- : int list = [3; 1; 2; 4; 3]

# remove_adjacents [2;2;2;2;2] ;;
- : int list = [2]


remove_adjacents [1;5;2;3;1;2;2;4;4;5] ;;
- : int list = [1; 5; 2; 3; 1; 2; 4; 5]


*)





(* Exercise  8b: 

Modify the function below (you can create a new function called duplicates) so that it 
removes duplicates from all lists, not just sorted list. The List.mem function might be helpful 

*)






(* 

Higher-Order Functions 

*)



(* Exercise  9: 


Define map (on int lists): (int -> int) -> int list -> int list 


*)



(* Exercise 10: 


Redefine the  square_all (squares a list of integers) and prods (mutliplies elements of a tuple in a list of tuples) functions using List.map

*)



(* Exercise 11: 


Define filter : ('a -> bool) -> 'a list -> 'a list

*)


(*

Exercise 12 : 


Implement fold_left : (int -> int -> int) -> int -> int list -> int 

*)


(*

Exercise 13 : 

Define a function sum that accepts a list of integers and returns their sum.

*)


(*

Exercise 14 : 

Implement filter : (int -> bool) -> int list -> int list using fold_left and fold_right


*)





(* 

Unit Testing: Below are examples of Unit Tests 

*)

module Ints = Set.Make(Int)

let rec factorial (n : int) : int = 
    if n = 0 then 1 
    else n * factorial (n - 1) ;;



let factorial_test () : unit =
    unit_test (factorial 0 = 1 )
               "factorial 0";
    unit_test (factorial 1 = 1)
               "factorial 1";
    unit_test (factorial 2 = 2)
               "factorial 2";
    unit_test (factorial 3 = 6)
              "factorial 3"  ;;
    

 

(* let remove_adjacents_test () : unit =
  unit_test (remove_adjacents [] = [] )
            "duplicates empty";
  unit_test (remove_adjacents [7] = [7])
            "duplicates single";
  unit_test (remove_adjacents [1;5;2;3;1;2;2;5] = [1;5;2;3;1;2;5])
            "duplicates repeat";
  unit_test (remove_adjacents [1;2;3;4;5] = [1;2;3;4;5])
            "duplicates same"
;;  *)




let test_all () : unit =
    (* remove_adjacents_test ();  *)
    factorial_test ();

    ;;
            
            
            
let _ = test_all () ;;



(* Design and Style *)


(* Exercise 15 

What's wrong with the following functions? Try to improve it

*)


let add t = 
    match t with 
    | (x, y) -> (x + y) ;;


let rec add_tuple_list l = 
    match l with 
    | [] -> []
    | a :: b -> 
        match a with 
        | (x, y) -> (x + y) :: add_tuple_list b ;; 


(*  Exercise 16

How would you improve the following function? 

*)

let rec max_list (lst : int list) : int =
  match lst with
  | [elt] -> elt
  | head :: tail ->
      if head > (max_list tail) then head
      else max_list tail ;;


