(*
                              CS51 Lab 13
                   Procedural Programming and Loops
 *)

(*
Objective:

This lab introduces and provides practice with:
  - loops and procedural programming
  - tail recursion as an alternate form of iteration.
 *)

(*====================================================================
Part 1: Revisiting list operations

In Lab 2, you created recursive functions to find the lengths and sums
of lists. Below are examples of both:

    let rec length (lst : int list) : int =
      match lst with
      | [] -> 0
      | _hd :: tl -> 1 + length tl ;;

    let rec sum (lst : int list) : int =
      match lst with
      | [] -> 0
      | hd :: tl -> hd + sum tl ;;

As noted in the book, these functional recursive implementations
may run into stack overflow errors when called on exceptionally long
lists. 

    # sum (List.init 1_000_000 Fun.id) ;;
    Stack overflow during evaluation (looping recursion?).

As computation proceeds, each recursive call of `length tl` or `sum
tl` is suspended, with the suspended computation stored on a stack,
until we reach the end of the list. At that point, the stack finally
unwinds and the expression is evaluated. If the number of calls grows
too large, we run out of room allocated to the stack of suspended
computations, and the computation fails with a `Stack_overflow`
exception.

Two ways of addressing this problem are (i) using a tail recursive
function or (ii) using an explicit loop.

In a tail recursive function the recursive invocation *is* the final
result of the invoking call. The value of the recursive function is
immediately returned; no further computation must be done to it, so no
suspended computation needs to be stored on the call stack, thus
avoiding the problem of stack overflow. 

Below, the length function above has been rewritten to use a
tail-recursive auxiliary function `length_tr` (the "tr" stands for
"tail-recursive") to demonstrate this:

    let length lst =
      let rec length_tr lst accum =
        match lst with
        | [] -> accum
        | _hd :: tl -> length_tr tl (1 + accum) in
      length_tr lst 0 ;;

The technique used here, using a tail-recursive auxiliary function
that makes use of an added argument that acts as an accumulator for
the partial results, is a common one for converting functions to
tail-recursive form.

......................................................................
Exercise 1: Tail-recursive sum 

Rewrite the `sum` function to be tail recursive. (As usual, for this
and succeeding exercises, you shouldn't feel beholden to how the
definition is introduced in the skeleton code below. For instance, you
might want to add a `rec`, or use a different argument list, or no
argument list at all but binding to an anonymous function instead.)

Verify your implementation is tail-recursive by summing up the
elements of an extremely long list, like this 1,000,000 element list:

    # sum (List.init 1_000_000 Fun.id) ;;
    - : int = 499999500000

Gauss would be proud!
....................................................................*)


  let sum (l : int list) : int =
    let rec sum_tr (acc : int) (lst : int list) : int = 
      match lst with 
      | [] -> acc 
      | hd :: tl -> sum_tr (hd + acc) tl
    in sum_tr 0 l
  
(*....................................................................
Exercise 2: Write a tail-recursive function `prods` that finds the
product of the corresponding elements of two integer lists. Your
function should raise a `Failure` exception when called on lists of
differing length. You may remember implementing a similar function in
Lab 2. It should work like this:

   # prods [1; 2; 3] [1; 2; 3] ;;
   -: int list = [1; 4; 9] 

Your initial try at a tail-recursive function may output a list that
is in reverse order of your expected output. This is a common outcome
in tail-recursive list iteration functions. (In general, you'd want to
consider whether or not this has negative outcomes on your intended
use case. It may be that the output order is not significant.)

In this case, for testing purposes, please preserve the initial
ordering of the lists. One method to do so is simply to reverse the
list at the end, using a tail-recursive reversal function of
course. Fortunately, the built-in `List.rev` is tail-recursive (even
though the documentation doesn't mention that fact).
....................................................................*)

let prods (l1 : int list) (l2 : int list) : int list =
  let rec prods_tr (acc: int list) (lst1 : int list) (lst2 : int list) : int list =
      match lst1, lst2 with 
      | [], [] -> acc
      | _, [] | [], _ -> failwith "Lists should be the same length"
      | hd1 :: tl1, hd2 :: tl2 -> prods_tr (hd1 * hd2 :: acc) tl1 tl2 
  in List.rev (prods_tr [] l1 l2)
;;

(*....................................................................
Exercise 3: Modify your `prods` function to use option types to deal
with lists of different lengths. Call it `prods_opt`. The `prods_opt`
function should return `None` if its two list arguments are of
different lengths, and if the lists are of the same length, `Some lst`
where `lst` is the products of the corresponding elements of its
arguments.
....................................................................*)

let prods_opt (l1 : int list) (l2 : int list) : int list option =
  let rec prods_tr (acc: int list) (lst1 : int list) (lst2 : int list) : int list option =
      match lst1, lst2 with 
      | [], [] -> Some acc
      | _, [] | [], _ -> None
      | hd1 :: tl1, hd2 :: tl2 -> prods_tr (hd1 * hd2 :: acc) tl1 tl2 
  in match prods_tr [] l1 l2 with 
  | Some(lst) -> Some (List.rev lst)
  | _ -> None
;;

(*....................................................................
Exercise 4: Finally, combine your `sum` and `prods` functions to
create a tail-recursive dot product function (that is, the sum of the
products of corresponding elements of the lists). (For reference, you
implemented dot product in lab 2.)
....................................................................*)



let dotprod (l1 : int list) (l2 : int list) : int =
  sum (prods l1 l2)
;;

(*====================================================================
Part 2: Loops

Another method of solving the problem of stack overflow when dealing
with large lists is to use loops. Unlike tail recursion, loops rely on
state change in order to function, and therefore go beyond the now
familiar purely functional paradigm, bringing us into the domain of
procedural programming.

Let's get some practice with simple loops.

......................................................................
Exercise 5: Write two non-recursive functions, `odd_while` and
`odd_for`, that use `while` and `for` loops, respectively, to return a
list of all positive odd numbers less than or equal to a given
int. (Don't worry about dealing with negative arguments.)

For example, we expect the following behavior:

  # odd_while 10
  - : int list = [1; 3; 5; 7; 9]
  # odd_for 7
  - : int list = [1; 3; 5; 7]
....................................................................*)

let odd_while (x : int) : int list =
  let counter = ref (if x mod 2 = 0 then x - 1 else x) and results = ref [] in 
    while !counter >= 1 do 
      results := !counter :: !results;
      counter := !counter - 2
    done;
    !results
;;


let odd_for (x : int) : int list =
  let results = ref [] in 
   for counter = x downto 1 do 
    if counter mod 2 = 1 then results := counter :: !results else ()
   done;
   !results
  ;;

(* Here is the `length` function implemented using a `while` loop, as
in the reading:

    let length_iter (lst : 'a list) : int =
      let counter = ref 0 in        (* initialize the counter *)
      let lstr = ref lst in         (* initialize the list *)
      while !lstr <> [] do          (* while list not empty... *)
        counter := succ !counter;   (*   increment the counter *)
        lstr := List.tl !lstr       (*   drop element from list *)
      done;
      !counter ;;                   (* return the counter value *)

Note that both the counter for the loop and the list need to be
references. Otherwise, their values can't be changed and the loop
will never terminate.

......................................................................
Exercise 6: Rewrite the functional recursive `sum` function from above
using a `while` loop.
....................................................................*)

let sum_iter (lst : int list) : int =
  failwith "sum_iter not implemented" ;;

let sum_iter (l : int list) : int =
  let lst = ref l and sum = ref 0 in 
  while !lst <> [] do 
    match !lst with 
    | hd :: tl -> sum := hd + !sum; lst := tl
    | _ -> failwith "Logic error: list shouldn't be empty"
  done;
  !sum
;;

(*....................................................................
Exercise 7: Rewrite the recursive `prods` function from above using a
`while` loop. Don't forget to handle the case where the two lists
have different lengths, by raising an appropriate exception.
....................................................................*)

let prods_iter (xs : int list) (ys : int list) : int list =
  let results = ref [] and left = ref xs and right = ref ys in 
  while !left <> [] && !right <> [] do 
    match !left, !right with 
    | h1 :: t1, h2 :: t2 -> results := (h1 * h2) :: !results; left := t1; right := t2
    | _ -> failwith "Logic error"
  done;
  if !left <> [] || !right <> [] then failwith "Lists were different lengths"
  else List.rev !results
;;

(* You've now implemented `prods` a few times, so think about which of
them you think is the most efficient, and which of them required the
most work to write. Remember the famous quotation from computer
scientist Donald Knuth: "We should forget about small efficiencies,
say about 97% of the time: premature optimization is the root of all
evil."

Tail recursion is a type of optimization, and it may not always be
worth the sacrifice in development time and code readability.
Iterative solutions in a functional programming language like OCaml
may also not be worth the time. It is critical to assess the impact
that your use cases will have on your design. *)

(*....................................................................
Exercise 8: You'll now reimplement one last familiar function:
reversing a list. Write a non-recursive function `reverse` to reverse
a list. (This function is implemented tail-recursively in the `List`
module as `List.rev` (see
https://github.com/ocaml/ocaml/blob/trunk/stdlib/list.ml), and you've
likely used it in previous exercises.)
....................................................................*)

let reverse (lst : 'a list) : 'a list =
  let result = ref [] and l = ref lst in 
  while !l <> [] do 
    result := (List.hd !l) :: !result;
    l := List.tl !l
  done;
  !result
 ;;

(* As you've observed in this lab, procedural programming can be
useful, but most problems can and typically should be solved with
functional techniques. However, there is one famous problem that you
may be familiar with a procedural implementation of: CS50's
Mario. (For those unfamiliar, the task is to print a half-pyramid of
lines of a given height as shown in the example below. See
https://docs.cs50.net/2017/ap/problems/mario/less/mario.html.) *)

(*....................................................................
Exercise 9: Implement a function in the procedural paradigm that
prints out a half-pyramid of a specified height, per the below. Use
hashes (#) for blocks. The function should raise an Invalid_argument
exception for heights greater than 23. (Why? I don't know. That's just
how CS50 did it.)

    # mario 5 ;;
        ##
       ###
      ####
     #####
    ######
    - : unit = ()

    # mario 24
    Exception: Invalid_argument "This pyramid is way too high for Mario".

....................................................................*)



let mario (height : int) : unit =
  if height > 23 || height < 0 then raise (Invalid_argument "Height must be 0 <= ht <= 23")
  else 
    
    let print_hashes (ht : int) (level : int) : unit =
      for counter = 0 to ht do 
        if counter >= ht - level then Printf.printf "#" else Printf.printf " "
      done;
      Printf.printf "\n" in
    
    for lvl = 1 to height do 
    print_hashes height lvl
  done;;
