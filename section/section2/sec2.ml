(*..................................... 
Records
.....................................*)

(* Exercise 1 


Define types student_tuple and student_recd 

We want to store a student's first name, last name, house, and grad year

......................*)

type student_recd = {
    first_name: string;
    last_name: string;
    house: string;
    graduation_year: int
}

type student_tuple = string * string * string * int

(* 

Exercise 2: 

Define two functions: 

(1) is_year_tuple accepts a student_tuple and a year and returns true if the student is 
in that year

(2) is_year_recd accepts a student_recd and a year and returns true if the student is in the

year 

*)


let is_year_tuple ((_, _, _, graduation_year) : student_tuple) (year: int) =
    year = graduation_year
;;

let is_year_recd (student : student_recd) (year: int) =
    year = student.graduation_year
;;


(*...............................

Polymorphism 
.................................*)



(* 

Exercise 3 

Implement the following higher-order functions and then use them in the following problem. 

(a) Implement map

    (i) Define a function floats_of_ints that accepts a list of integers and converts it into a list of floating-point numbers.

(b) Implement filter 

    (i) Define a function positive_pairs that accepts a list of point_pairs 

        and returns a list of only those pairs whose x and y values are both positive.

          type point_pair = int * int 


(c) Implement fold_left and fold_right 

    (i) Define a function reverse that reverses a list of integers

*)

let rec map (action : 'a -> 'b) (lst: 'a list) : 'b list =
    match lst with
    | [] -> []
    | head :: tail -> action head :: (map action tail)
;;

let floats_of_ints: float list -> int list = map int_of_float ;;

let rec filter (action : 'a -> bool) (lst: 'a list) : 'a list =
    match lst with
    | [] -> []
    | head :: tail -> if action head then head :: (filter action tail) else filter action tail
;;

let rec fold_left (f : 'a -> 'b -> 'a) (accumulator : 'a) (lst: 'b list) : 'a =
    match lst with
    | [] -> accumulator
    | head :: tail -> fold_left f (f accumulator head) tail
;;

let rec fold_right (f : 'a -> 'b -> 'b) (lst: 'a list) (accumulator: 'b) : 'b =
    match lst with
    | [] -> accumulator
    | head :: tail -> f head (fold_right f tail accumulator)
;;

(* 

Exercise 3b: For each of the following definitions of a function f, give its most general type (as would
be inferred by OCaml) without use utop or explain why no type exists for this function. Don't use utop!


1. let f x =
x +. 42. ;;

float -> float

2. let f g x =
g (x + 1) ;;

(int -> 'a) -> int -> 'a

3. let f x =
match x with
| [] -> x
| h :: t -> h ;;


4. let rec f x a =
match x with
| [] -> a
| h :: t -> h (f t a) ;;)

5. let f x y =
match x with
| (w, z) -> if w then y z else w ;;

6. let f x y =
x y y ;;

7. let f x y =
x (y y) ;;

8. let rec f x =
match x with
| None
| Some 0 -> None
| Some y -> f (Some (y - 1)) ;;

9. let f x y =
if x then [x]
else [not x; y] ;;



*)



(*...............................

Higher-Order Functions  
.................................*)

(* Exercise 4 


(a) Provide an implementation of the List.map function over a list using only a call to
List.fold_right over the same list, or provide an argument for why it’s not possible to
do so

(b) Provide an implementation of the List.fold_right function using only a call to
List.map over the same list, or provide an argument for why it’s not possible to do so.

..............*)


(* Exercise 5

Implement the following four functions. Try implementing them one way using map, fold, and filter and try without

(a) square_sum (square all elements in list and sum)

(b) increase (increase all elements in a list by 2.5)

(c) special_sum (sum all elements that are greater than 5)

(d) is_mult_3 (pick only elements in list that are a multiple of 3)

..............*)



(*


Exercise 6

Provide an OCaml definition for a higher-order function @+ that takes two functions
as arguments and returns their composition. The function should have the following
behavior:

# let weighted_sum = sum @+ prods ;;

val weighted_sum : (int * int) list -> int = <fun>

# weighted_sum [(1, 3); (2, 4); (3, 5)] ;;
- : int = 26

.....................*)

let (@+) (f : 'a -> 'b) (g : 'c -> 'a) : 'c -> 'b =
    fun x -> f (g x)
;;


(*..................

Currying vs Uncurrying 

....................*)

(*..................

Exercise 6 (from Lab)

(1) Write a polymorphic higher-order function curry that 

takes as its argument an uncurried binary function and 

returns the curried version of its argument function.


(2) Write a polymorphic higher-order function uncurry 

that takes as its argument a curried binary function and 

returns the uncurried version of its argument function.
....................*)


(*............................. 

Option Types

...........................*)


(* 


Exercise 7 

Define a function variance : float list -> float option that returns None
if the list has fewer than two elements. Otherwise, it should return the variance of the
numbers in its list argument, wrapped appropriately for its return type.

Formula for variance is shown on lecture slides or you can look it up 

If you want to compare your output
with an online calculator, make sure you
find one that calculates the (unbiased)
sample variance.

# variance [1.0; 2.0; 3.0; 4.0; 5.0] ;;
- : float option = Some 2.5
# variance [1.0] ;;
- : float option = None

Remember to use the floating point version of the arithmetic operators when operating
on floats (+., *., etc). The function float can convert (“cast”) an int to a float.

*)




(* 

Exercise 8: 

Now implement the zip function from lab with option types. 

Define a function zip_opt such that zip_opt [1; 2] [3; 4] = Some [(1, 3); (2, 4)] 

and zip_opt returns None if lists are not the same length.

*)


(*..........................

Exceptions

..............................*)

(* 

Exercise 9

Define an exception called Unequallists 

*)



(*

Exercise 10: Reimplment the zip function, raising an exception when lists are of different lengths

*)