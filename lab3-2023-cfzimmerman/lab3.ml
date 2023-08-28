(*
                               CS51 Lab 3
                     Polymorphism and record types
*)

(*
Objective:

In this lab, you'll exercise your understanding of polymorphism and
record types. Some of the problems extend those from Lab 2, but we'll
provide the necessary background code from that lab. *)

(*======================================================================
  Part 1: Records and tuples

  Records and tuples provide two different ways to package together
  data. They differ in whether their components are selected by *name*
  or by *position*, respectively.

  Consider a point in Cartesian (x-y) coordinates. A point is specified
  by its x and y values, which we'll take to be ints. We can package
  these together as a pair (a 2-tuple), as in the following data type
  definition: *)

type point_pair = int * int

(* Then, we can add two points (summing their x and y coordinates
   separately) with the following function:

       let add_point_pair (p1 : point_pair) (p2 : point_pair) : point_pair =
         let x1, y1 = p1 in
         let x2, y2 = p2 in
         (x1 + x2, y1 + y2) ;;

   ........................................................................
   Exercise 1:

   It might be nicer to deconstruct the points in a single match, rather
   than the two separate matches in the two let expressions. Reimplement
   `add_point_pair` to use a single pattern match in a single `let`
   expression.
   ......................................................................*)

let add_point_pair (p1 : point_pair) (p2 : point_pair) : point_pair =
  match (p1, p2) with (p1_x, p1_y), (p2_x, p2_y) -> (p1_x + p2_x, p1_y + p2_y)

(* Analogously, we can define a point by using a record to package up
   the x and y coordinates. *)

type point_recd = { x : int; y : int }

let r = { x = 1; y = 3 }

(*......................................................................
  Exercise 2A:

  Replace the two lines below with a single `let` expression that
  extracts the x and y coordinate values from `r` into `x1` and `y1`.
  ......................................................................*)

let x1 = r.x
let y1 = r.y

(*......................................................................
  Exercise 2B:

  Implement a function `add_point_recd` to add two points of type
  `point_recd` and returning a `point_recd` as well.
  ......................................................................*)

let add_point_recd ({ x = p1_x; y = p1_y } : point_recd)
    ({ x = p2_x; y = p2_y } : point_recd) : point_recd =
  { x = p1_x + p2_x; y = p1_y + p2_y }

(* Recall the dot product from Lab 2. The dot product of two points
   `x1, y1` and `x2, y2` is the sum of the products of their x and y
   coordinates.

   ........................................................................
   Exercise 3: Write a function `dot_product_pair` to compute the dot
   product for points encoded as the `point_pair` type.
   ......................................................................*)

let dot_product_pair ((p1_x, p1_y) : point_pair) ((p2_x, p2_y) : point_pair) :
    int =
  (p1_x * p2_x) + (p1_y * p2_y) ;;

(*......................................................................
  Exercise 4: Write a function `dot_product_recd` to compute the dot
  product for points encoded as the `point_recd` type.
  ......................................................................*)

let dot_product_recd (p1 : point_recd) (p2 : point_recd) : int =
  p1.x * p2.x + p1.y * p2.y ;;

(* Converting between the pair and record representations of points

   You might imagine that the two representations have
   different advantages, such that two libraries, say, might use
   differing types for points. In that case, we may want to have
   functions to convert between the two representations.

   ........................................................................
   Exercise 5: Write a function `point_pair_to_recd` that converts a
   `point_pair` to a `point_recd`.
   ......................................................................*)

let point_pair_to_recd ((x, y) : point_pair) = { x; y }

(*......................................................................
  Exercise 6: Write a function `point_recd_to_pair` that converts a
  `point_recd` to a `point_pair`.
  ......................................................................*)

let point_recd_to_pair ({ x; y } : point_recd) : point_pair = (x, y)

(*======================================================================
  Part 2: A simple database of records

  A college wants to store student records in a simple database,
  implemented as a list of individual course enrollments. The enrollments
  themselves are implemented as a record type, called `enrollment`, with
  `string` fields labeled `name` and `course` and an integer student ID
  number labeled `id`. The appropriate type definition is: *)

type enrollment = { name : string; id : int; course : string }

(* Here's an example of a list of enrollments. *)

let college =
  [
    { name = "Pat"; id = 603858772; course = "cs51" };
    { name = "Pat"; id = 603858772; course = "expos20" };
    { name = "Pat"; id = 603858772; course = "ec10b" };
    { name = "Kim"; id = 482958285; course = "expos20" };
    { name = "Kim"; id = 482958285; course = "cs20" };
    { name = "Sandy"; id = 993855891; course = "ls1b" };
    { name = "Sandy"; id = 993855891; course = "cs51" };
    { name = "Sandy"; id = 482958285; course = "ec10b" };
  ]

(* In the following exercises, you'll want to avail yourself of the
   `List` module functions, writing the requested functions in
   higher-order style rather than handling the recursion yourself.

   ........................................................................
   Exercise 7: Define a function called `transcript` that takes an
   `enrollment list` and returns a list of all the enrollments for a given
   student as specified by the student's ID.

   For example:

       # transcript college 993855891 ;;
       - : enrollment list =
       [{name = "Sandy"; id = 993855891; course = "ls1b"};
        {name = "Sandy"; id = 993855891; course = "cs51"}]
   ......................................................................*)

let transcript (enrollments : enrollment list) (student : int) : enrollment list
    =
  List.filter (fun row -> row.id = student) enrollments

(*......................................................................
  Exercise 8: Define a function called `ids` that takes an `enrollment
  list` and returns a list of all the ID numbers in that list,
  eliminating any duplicates. Hint: The `map` and `sort_uniq` functions
  from the `List` module and the `compare` function from the `Stdlib`
  module may be useful here.

  For example:

      # ids college ;;
      - : int list = [482958285; 603858772; 993855891]
  ......................................................................*)

let ids (enrollments : enrollment list) : int list =
  List.sort_uniq compare (List.map (fun row -> row.id) enrollments)

(*......................................................................
  Exercise 9: Define a function `verify` that determines whether all the
  entries in an enrollment list for each of the ids appearing in the
  list have the same name associated. Hint: You may want to use
  functions from the `List` module such as `map`, `for_all`,
  `sort_uniq`.

  For example:

      # verify college ;;
      - : bool = false

      for each unique id,
  ......................................................................*)

(*
   verify:
    - Accepts a list of enrollment records and returns whether or not entries have the same ID but different names.
    - Given the head of a list, partition the list into two lists. In the first, every row has the same ID as the head. The second is every row that doesn't. 
    - For every row that has the same ID, filter out every row that has the same name as head. If there are any rows left over that don't have the same name, return false. 
    - Otherwise, verify the rest of the list on the side without a matching ID.
*)
let rec verify (enrollments : enrollment list) : bool =
  match enrollments with
  | [] -> true
  | head :: tail ->
      let matches, non_matches =
        List.partition (fun row -> row.id = head.id) tail
      in
      List.length (List.filter (fun row -> row.name <> head.name) matches) = 0
      && verify non_matches
    ;;

(*======================================================================
  Part 3: Polymorphism

  ........................................................................
  Exercise 10: In Lab 2, you implemented a function `zip` that takes two
  lists and "zips" them together into a list of pairs. Here's a possible
  implementation of `zip`:

    let rec zip (x : int list) (y : int list) : (int * int) list =
      match x, y with
      | [], [] -> []
      | xhd :: xtl, yhd :: ytl -> (xhd, yhd) :: (zip xtl ytl) ;;

  As implemented, this function works only on integer lists. Revise your
  solution to operate polymorphically on lists of any type. What is the
  type of the result? Did you provide full typing information in the
  first line of the definition? (As usual, for the time being, don't
  worry about explicitly handling the anomalous case when the two lists
  are of different lengths.)
  ......................................................................*)

let rec zip (x : 'a list) (y : 'b list) : ('a * 'b) list =
  match (x, y) with
  | [], [] | _, [] | [], _ -> []
  | x_head :: x_tail, y_head :: y_tail -> (x_head, y_head) :: zip x_tail y_tail

(*......................................................................
  Exercise 11: Partitioning a list -- Given a boolean function, say

      fun x -> x mod 3 = 0

  and a list of elements, say,

      [3; 4; 5; 10; 11; 12; 1]

  we can partition the list into two lists, the list of elements
  satisfying the boolean function (`[3; 12]`) and the list of elements
  that don't (`[4; 5; 10; 11; 1]`).

  The library function `List.partition` partitions its list argument in
  just this way, returning a pair of lists. Here's an example:

      # List.partition (fun x -> x mod 3 = 0) [3; 4; 5; 10; 11; 12; 1] ;;
      - : int list * int list = ([3; 12], [4; 5; 10; 11; 1])

  What is the type of the `partition` function, keeping in mind that it
  should be as polymorphic as possible?

  Now implement the function yourself (without using `List.partition`,
  though other `List` module functions may be useful).
  ......................................................................*)

let rec partition (f : 'a -> bool) (lst : 'a list) : 'a list * 'a list =
  match lst with
  | [] -> ([], [])
  | head :: tail ->
      let matches, non_matches = partition f tail in
      if f head then (head :: matches, non_matches)
      else (matches, head :: non_matches)

(*......................................................................
  Exercise 12: We can think of function application itself as a
  higher-order function (!!!). It takes two arguments -- a function and
  its argument -- and returns the value obtained by applying the
  function to its argument. In this exercise, you'll write this
  function, called "apply". You might use it as in the following
  examples:

      # apply pred 42 ;;
      - : int = 41
      # apply (fun x -> x ** 2.) 3.14159 ;;
      - : float = 9.86958772809999907

  (You may think such a function isn't useful, since we already have an
  even more elegant notation for function application, as in

      # pred 42 ;;
      - : int = 41
      # (fun x -> x ** 2.) 3.14159 ;;
      - : float = 9.86958772809999907

  But we'll see a quite useful operator that works similarly -- the
  backwards application operator -- in Chapter 11 of the textbook.)

  Start by thinking about the type of the function. We'll assume it
  takes its two arguments curried, that is, one at a time.

  1. What is the most general (polymorphic) type for its first argument
     (the function to be applied)?

  2. What is the most general type for its second argument (the argument
     to apply it to)?

  3. What is the type of its result?

  4. Given the above, what should the type of the function `apply` be?

  Now write the function.

  Can you think of a reason that the `apply` function might in fact be
  useful?
  ......................................................................*)

let apply (f : 'a -> 'b) (value : 'a) : 'b = f value

;;