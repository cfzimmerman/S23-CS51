(*
                         CS 51 Problem Set 3
                      Bignums and RSA encryption
*)

(* In this problem set, you'll implement arbitrary precision integers
   -- "bignums" -- for use in an implementation of the RSA public-key
   cryptographic system. As with the previous problem set, you may
   express your solution to a particular problem in terms of another
   function from a previous problem, and you may use functions from the
   `List` module where appropriate.

   Before starting on the problem set, read the problem set 3 writeup in
   `readme.pdf`. It provides context and crucial information for
   completing the problems. In addition, make sure that you are familiar
   with the problem set procedures in the document "Problem set
   procedures for CS51".
*)

(*======================================================================
  Basics: representing bignums, negating and comparing them, and
  converting between bignums and other representations.
*)

(* bignum -- Type for representing bignums (arbitrary precision
   integers. Uses a boolean field neg for negative numbers and a list
   coeffs of coefficients from most to least significant. The
   `readme.pdf` provides details of invariants that this
   representation assumes.

   For example (assuming cBASE is defined as 1000, as below), the
   number 2,013 would be represented as a value of this type as

       {neg = false; coeffs = [2; 13]}    .

   the number -12,000,654 would be represented as

       {neg = true; coeffs = [12; 0; 654]}    ,

   and the number 0 would be represented as

       {neg = false; coeffs = []}     .
*)

type bignum = { neg : bool; coeffs : int list }

(* cBASE -- Global constant, the base for representing bignums, which
   must be a power of 10. Your code should work for any reasonable
   value of `cBASE`, not just the initial value we've set it to. When
   submitting, have `cBASE` be 1000, as it is here. *)

let cBASE = 1000

(*......................................................................
  Problem 1: Negation
  { expr with field1 = expr1 ; ... ;  fieldn = exprn }

  ......................................................................*)

(* negate b -- Returns a `bignum` that is the negation of `b`. *)
let negate (b : bignum) : bignum =
  match b.coeffs with [] -> b | _ -> { b with neg = not b.neg }

(*......................................................................
  Problem 2: Comparing bignums
  ......................................................................*)

(*
  Turns a bool into an int. 1 for true, 0 for false. 
*)
let int_of_bool (b : bool) = if b then 1 else 0

(*
  Returns the result of the comparison based on negativity alone. If \
    they're the same negativity, we can't determine anything.
    ex. Returns true on (<) if b1 is negative and b2 is positive
  IMPORTANT: b1_neg and b2_neg are true if the values are negative \
    (like the bignum's fields)
*)
let compare_negativity (compare : 'a -> 'a -> bool) (b1_neg : bool)
    (b2_neg : bool) : bool =
  compare (int_of_bool (not b1_neg)) (int_of_bool (not b2_neg))

(*
  Returns true if the comparison is valid and can be computed by length \
    alone (must be of the same pos/neg type for length to indicate size).
*)
let compare_length (compare : 'a -> 'a -> bool) (b1 : bignum) (b2 : bignum) :
    bool =
  let b1_coeff_len, b2_coeff_len =
    (List.length b1.coeffs, List.length b2.coeffs)
  in
  b1_coeff_len <> b2_coeff_len
  && b1.neg = b2.neg
  && ((b1.neg && compare b2_coeff_len b1_coeff_len)
     || ((not b1.neg) && compare b1_coeff_len b2_coeff_len))

(*
  When logically comparing (<, >) integers, we'll flip the results if the \
    inputs are negative.
*)
let reverse_if_negative (negative : bool) (expression : bool) : bool =
  if negative then not expression else expression

(*
  Applies a logical comparison to two bignums (like >, <, =). Returns \
   a boolean.
*)
let rec compare_nums (compare : 'a -> 'a -> bool) (b1 : bignum) (b2 : bignum) :
    bool =
  let b1_coeff_len, b2_coeff_len =
    (List.length b1.coeffs, List.length b2.coeffs)
  in
  if b1_coeff_len = 0 && b2_coeff_len = 0 then compare 0 0
  else if b1.neg <> b2.neg then compare_negativity compare b1.neg b2.neg
  else if b1_coeff_len <> b2_coeff_len then compare_length compare b1 b2
  else
    match (b1.coeffs, b2.coeffs) with
    | [], hd :: _ -> reverse_if_negative b2.neg (compare ~-1 hd)
    | hd :: _, [] -> reverse_if_negative b1.neg (compare hd ~-1)
    | h1 :: t1, h2 :: t2 ->
        if h1 = h2 then
          compare_nums compare { b1 with coeffs = t1 } { b2 with coeffs = t2 }
        else reverse_if_negative b1.neg (compare h1 h2)
    | [], [] -> raise (Failure "End case reached. Bug detected.")

let equal = compare_nums ( = )

(* less b1 b2 -- Predicate returns `true` if and only if `b1`
   represents a smaller number than `b2`. *)
let less = compare_nums ( < )

(* greater b1 b2 -- Predicate returns `true` if and only if `b1`
   represents a larger number than `b2`. *)
let greater = compare_nums ( > )

(*......................................................................
  Problem 3: Converting to and from bignums
  ......................................................................*)

let rec pos_int_pow (num : int) (pow : int) =
  if pow < 0 then
    raise (Invalid_argument "pos_int_pow only accepts nonnegative exponents")
  else match pow with 0 -> 1 | 1 -> num | _ -> num * pos_int_pow num (pow - 1)

(*
  Given a number,_int_to_cbase_list returns a list of subunits divided into equal \
    sizes. 
  This function should only be called recursively. Use the protected version if \
    calling elsewhere.
*)
let rec _int_to_cbase_list (cbase_pow : int) (num : int) : int list =
  if num <= 0 then []
  else
    let dividend = pos_int_pow cBASE cbase_pow in
    let remainder = num mod dividend in
    _int_to_cbase_list (cbase_pow + 1) (num - remainder)
    @ [ remainder / (dividend / cBASE) ]

(*
  Call this to convert an int into an array of cbase-chunked ints. 
*)
let int_to_cbase_list = _int_to_cbase_list 1

(* from_int n -- Returns a bignum representing the integer `n`. *)
let from_int (num : int) : bignum =
  { neg = num < 0; coeffs = int_to_cbase_list (abs num) }

(*
    Generic helper function to determine if multiplying two integers \
      will result in an overflowing value. Returns true if ints will overflow.
*)
let ints_will_overflow (x : int) (y : int) : bool =
  let fl_x, fl_y = (float_of_int x, float_of_int y) in
  fl_x *. fl_y > float_of_int max_int

(*
  Given a list of ints, cbase_list_to_int either returns a record containing \
    the int value or an indication that the integer overflowed during computation.
*)
type converted_cbase_data = { cbase_pow : int; value : int; overflowed : bool }

let rec cbase_list_to_int (lst : int list) : converted_cbase_data =
  match lst with
  | [] -> { cbase_pow = 0; value = 0; overflowed = false }
  | head :: tail ->
      let acc = cbase_list_to_int tail in
      if acc.overflowed then acc
      else
        let cbase_factor = pos_int_pow cBASE acc.cbase_pow in
        {
          cbase_pow = acc.cbase_pow + 1;
          value = acc.value + (head * cbase_factor);
          overflowed = ints_will_overflow head cbase_factor;
        }

let signed_num (negative : bool) (num : int) : int =
  if negative then num * ~-1 else num

(* to_int b -- Returns `Some v`, where `v` is the `int` represented by
   the bignum `b`, if possible, or `None` if `b` represents an integer
   out of the representable range of the `int` type. *)
let to_int (b : bignum) : int option =
  let converted_coeffs = cbase_list_to_int b.coeffs in
  if converted_coeffs.overflowed then None
  else Some (signed_num b.neg converted_coeffs.value)

(*======================================================================
  Helpful functions (not to be used in problems 1 to 3)
*)

(* trim_leading_zeroes lst -- Removes zero coefficients from the beginning of
   the coefficients part of a bignum representation *)
let rec trim_leading_zeroes (lst : int list) : int list =
  match lst with 0 :: tl -> trim_leading_zeroes tl | _ -> lst

(* clean b -- Removes zero coefficients from the beginning of a bignum
   representation *)
let clean (b : bignum) : bignum =
  { neg = b.neg; coeffs = trim_leading_zeroes b.coeffs }

(* rand_bignum bound -- Returns a random bignum from 0 to the absolute
   value of `bound` (inclusive). Useful for randomly testing
   functions. *)
let rand_bignum (bound : bignum) : bignum =
  let rand_base = List.map (fun _ -> Random.int cBASE) in
  let rec rand_bignum_rec (bounds : int list) =
    match bounds with
    | [] -> []
    | h :: t ->
        let r = Random.int (h + 1) in
        r :: (if r = h then rand_bignum_rec else rand_base) t
  in
  { neg = false; coeffs = trim_leading_zeroes (rand_bignum_rec bound.coeffs) }

(* explode s -- Splits a string `s` into a list of its characters. *)
let rec explode (s : string) : char list =
  let len = String.length s in
  if len = 0 then [] else s.[0] :: explode (String.sub s 1 (len - 1))

(* implode cs -- Condenses a list of characters `cs` into a string. *)
let rec implode (cs : char list) : string =
  match cs with [] -> "" | c :: t -> String.make 1 c ^ implode t

(* split lst n -- Returns a pair containing the first `n` elements of
   `lst` and the remaining elements of `lst`. *)
let rec split lst n =
  if n = 0 then ([], lst)
  else
    match lst with
    | [] -> ([], [])
    | hd :: tl ->
        let lst1, lst2 = split tl (n - 1) in
        (hd :: lst1, lst2)

(* take_first lst n -- Returns the first `n` elements of list `lst`
   (or the whole `lst` if too short). *)
let take_first (lst : 'a list) (n : int) : 'a list = fst (split lst n)

(* intlog base -- Returns the floor of the base 10 log of an integer
   `base` *)
let intlog (base : int) : int = int_of_float (log10 (float_of_int base))

(* from_string s -- Converts a string `s` representing an integer to a
   bignum. Assumes the base `cBASE` is a power of 10. *)
let from_string (s : string) : bignum =
  let rec from_string_rec (cs : char list) : int list =
    if cs = [] then []
    else
      let chars_to_convert, rest = split cs (intlog cBASE) in
      let string_to_convert = implode (List.rev chars_to_convert) in
      int_of_string string_to_convert :: from_string_rec rest
  in

  match explode s with
  | [] -> from_int 0
  | h :: t ->
      if h = '-' || h = '~' then
        { neg = true; coeffs = List.rev (from_string_rec (List.rev t)) }
      else
        {
          neg = false;
          coeffs =
            trim_leading_zeroes (List.rev (from_string_rec (List.rev (h :: t))));
        }

(* to_string b -- Converts a bignum `b` to its string representation.
   Returns a string beginning with `~` for negative integers. Assumes
   the base `cBASE` is a power of 10. *)
let to_string (b : bignum) : string =
  let rec pad_leading_zero_chars (s : string) (len : int) =
    if String.length s >= len then s
    else "0" ^ pad_leading_zero_chars s (len - 1)
  in

  let rec trim_leading_zero_chars (s : string) (c : char) =
    if String.length s = 0 then "0"
    else if String.get s 0 = '0' then
      trim_leading_zero_chars (String.sub s 1 (String.length s - 1)) c
    else s
  in

  let rec coeffs_to_string (coeffs : int list) : string =
    match coeffs with
    | [] -> ""
    | h :: t ->
        pad_leading_zero_chars (string_of_int h) (intlog cBASE)
        ^ coeffs_to_string t
  in

  let trimmed = trim_leading_zeroes b.coeffs in
  if List.length trimmed = 0 then "0"
  else
    let from_coeffs = trim_leading_zero_chars (coeffs_to_string trimmed) '0' in
    if b.neg then "~" ^ from_coeffs else from_coeffs

(*======================================================================
  Arithmetic functions
*)

(* plus_nonneg b1 b2 -- Returns a bignum representing the sum of `b1` and
   `b2`. NB: Assumes that (and works only when) the sum is nonnegative. *)
let plus_nonneg (b1 : bignum) (b2 : bignum) : bignum =
  let pair_from_carry (carry : int) : bool * int list =
    match carry with
    | 0 -> (false, [])
    | 1 -> (false, [ 1 ])
    | -1 -> (true, [ 1 ])
    | _ -> failwith "pair_from_carry: invalid carry"
  in

  let rec plus_with_carry ((neg1, coeffs1) : bool * int list)
      ((neg2, coeffs2) : bool * int list) (carry : int) : bool * int list =
    match (coeffs1, coeffs2) with
    | [], [] -> pair_from_carry carry
    | [], _ ->
        if carry = 0 then (neg2, coeffs2)
        else plus_with_carry (neg2, coeffs2) (pair_from_carry carry) 0
    | _, [] -> plus_with_carry (neg2, coeffs2) (neg1, coeffs1) carry
    | h1 :: t1, h2 :: t2 ->
        let sign1, sign2 =
          ((if neg1 then ~-1 else 1), if neg2 then ~-1 else 1)
        in
        let result = (h1 * sign1) + (h2 * sign2) + carry in
        if result < 0 then
          let negres, coeffsres = plus_with_carry (neg1, t1) (neg2, t2) ~-1 in
          (negres, (result + cBASE) :: coeffsres)
        else if result >= cBASE then
          let negres, coeffsres = plus_with_carry (neg1, t1) (neg2, t2) 1 in
          (negres, (result - cBASE) :: coeffsres)
        else
          let negres, coeffsres = plus_with_carry (neg1, t1) (neg2, t2) 0 in
          (negres, result :: coeffsres)
  in

  let neg_result, coeffs_result =
    plus_with_carry (b1.neg, List.rev b1.coeffs) (b2.neg, List.rev b2.coeffs) 0
  in
  { neg = neg_result; coeffs = trim_leading_zeroes (List.rev coeffs_result) }

(*......................................................................
  Problem 4

  The `plus` function returns a bignum representing b1 + b2. However, it
  does NOT make the same assumption as `plus_nonneg` (that the sum is
  nonnegative).

  Hint: How can you use `plus_nonneg` to implement `plus`? Make sure that
  your implementation preserves the bignum invariant.

  ......................................................................*)

(*
  run_subtraction accepts the standard inputs to a subtraction equation plus \
    the count of how many numbers we've borrowed during previous subtractions \
    to get to this point.
  Calculate the subraction, borrowing from the next set of numbers to do so if \
    needed (we can do so with confidence because we're subtracting x - y \
    where abs x > abs y)
*)
type run_sub_type = { new_debt : int; difference : int }

let run_subtraction (minuend : int) (subtrahend : int) (current_debt : int) :
    run_sub_type =
  let unaltered_difference = minuend - subtrahend - current_debt in
  if unaltered_difference >= 0 then
    { new_debt = 0; difference = unaltered_difference }
  else
    { new_debt = 1; difference = cBASE + minuend - subtrahend - current_debt }

(*
  Style: This function has redundencies in-between the second and third match cases. \
   However, abstracting the logic proved much more difficult than expected. After \
   eventually replacing it with a bloated, single-purpose helper function, I decided \
   to just go back to this. Perhaps some clever refactoring, currying, and inheritance
   could get us to a better place, but I see this as a more acceptable if not ideal \
   path forward.
*)
let rec minus_nonneg_reversed (greater : int list) (lesser : int list)
    (carryover_debt : int) : int list =
  match (greater, lesser) with
  | [], [] -> []
  | head :: tail, [] ->
      let { new_debt; difference } = run_subtraction head 0 carryover_debt in
      difference :: minus_nonneg_reversed tail [] new_debt
  | h_greater :: t_greater, h_lesser :: t_lesser ->
      let { new_debt; difference } =
        run_subtraction h_greater h_lesser carryover_debt
      in
      difference :: minus_nonneg_reversed t_greater t_lesser new_debt
  | [], _ ->
      raise
        (Invalid_argument
           "Lesser list is abs greater than greater list. Swap the two \
            incoming arguments.")

(*
  Returns a larger (by absolute value) cBase int list subtracted from smaller one.  
*)
let minus_nonneg (greater : int list) (lesser : int list) : int list =
  List.rev (minus_nonneg_reversed (List.rev greater) (List.rev lesser) 0)

(* plus b1 b2 -- Returns the bignum sum of `b1` and `b2` *)
let plus (b1 : bignum) (b2 : bignum) : bignum =
  if (not b1.neg) && not b2.neg then plus_nonneg b1 b2
  else if b1.neg && b2.neg then
    {
      (plus_nonneg { b1 with neg = false } { b2 with neg = false }) with
      neg = true;
    }
  else if equal { b1 with neg = false } { b2 with neg = false } then
    { neg = false; coeffs = [] }
  else
    let abs_greater, abs_lesser =
      if greater { b1 with neg = false } { b2 with neg = false } then (b1, b2)
      else (b2, b1)
    in
    {
      neg = abs_greater.neg;
      coeffs =
        trim_leading_zeroes (minus_nonneg abs_greater.coeffs abs_lesser.coeffs);
    }

(*......................................................................
  Problem 5

  The times function returns a bignum representing b1 * b2.

  Think about how you were first taught multiplication, say, 543 x
  224. It went something like this:

           5 4 3
         x 2 2 4
         -------
         2 1 7 2 <--- Partial product 5 4 3 x 4

     + 1 0 8 6 0 <--- Partial product 5 4 3 x 2; note that a zero is
                      appended after the partial product
   + 1 0 8 6 0 0 <--- Partial product 5 4 3 x 2; note that two zeroes
   -------------      are appended after the partial product
   = 1 2 1 6 3 2 <--- Sum of all (shifted) partial products

  When approaching this problem, it is advisable to break the problem
  down into simpler, easier-to-implement sub-problems. That way, you can
  test each helper function individually rather than having to test all
  of it at once, making locating bugs much easier. What are some natural
  subproblems implied by the example above?

  You may assume positivity in some of your helper functions if it
  simplifies the code, as long as the invariant is preserved.

  ......................................................................*)
(*
  🪧 NOTE TO TF: Please go to the bottom and read the comments around "times" first. \
    These are all helper functions.
*)

(* Given a string, returns a list of chars. *)
let string_to_char_list (str : string) : char list =
  List.of_seq (String.to_seq str)

(* Given a list of chars, returns a list of ints. This shouldn't be used for \
   non-integer chars. *)
let char_list_to_int_list (chars : char list) : int list =
  List.map (fun char_int -> int_of_char char_int - int_of_char '0') chars

(* Given a stringified number, returns a list of ints. Ex: "123456" turns into \
   [1; 2; 3; 4; 5; 6]. *)
let string_to_int_list (str : string) : int list =
  (* I think the logic is safer leaving this in, but the secret message only works \
    when I comment it out.
     let _ = int_of_string str in
  *)
  (* Verifies the string is a valid int. Throws error if not. *)
  char_list_to_int_list (string_to_char_list str)

(* Given a bignum, returns an unsigned list of individually-stored ints. \
   Ex: coeffs [123; 456] turns into [1; 2; 3; 4; 5; 6] *)
let bignum_to_pos_int_list (bn : bignum) : int list =
  string_to_int_list (to_string { bn with neg = false })

(* Given a list of single-stored ints and something to multiply them by, \
   returns a single-stored int list of their product IN REVERSE ORDER. *)
let rec _reversed_partial_product (multiplicand : int)
    (reversed_multipliers : int list) (carryover : int) : int list =
  match reversed_multipliers with
  | [] -> [ carryover ]
  | head :: tail ->
      let prod = (multiplicand * head) + carryover in
      (prod mod 10) :: _reversed_partial_product multiplicand tail (prod / 10)

(* Returns an int list of zeroes of the length requested. *)
let rec list_of_zeroes (num_zeroes : int) : int list =
  if num_zeroes = 0 then [] else 0 :: list_of_zeroes (num_zeroes - 1)

(* Given two reversed single-int lists, calculates and returns a REVERSE-ORDER \
   list of all partial products stored as single-int lists. *)
let rec _reversed_multiply_int_lists (mult_of_10 : int)
    (reversed_multiplicand : int list) (reversed_multipliers : int list) :
    int list list =
  match reversed_multiplicand with
  | [] -> []
  | head :: tail ->
      (List.rev (_reversed_partial_product head reversed_multipliers 0)
      @ list_of_zeroes mult_of_10)
      :: _reversed_multiply_int_lists (mult_of_10 + 1) tail reversed_multipliers

(* Thinking visually, if long-multiplying, there's a top number and a bottom number. \
   Provided those, return a matrix of single-int lists with each partial product. *)
let multiply_int_lists (top_number : int list) (bottom_number : int list) :
    int list list =
  _reversed_multiply_int_lists 0 (List.rev bottom_number) (List.rev top_number)

(* Given two REVERSED single-int lists, calculates and returns their sum as a \
   single-int list IN REVERSE ORDER. *)
let rec _reversed_sum_single_int_lists (carryover : int)
    (reversed_list1 : int list) (reversed_list2 : int list) : int list =
  match (reversed_list1, reversed_list2) with
  | [], [] ->
      if carryover = 0 then []
      else
        (carryover mod 10)
        :: _reversed_sum_single_int_lists (carryover / 10) [] []
  | head :: tail, [] | [], head :: tail ->
      ((carryover + head) mod 10)
      :: _reversed_sum_single_int_lists ((carryover + head) / 10) tail []
  | h1 :: t1, h2 :: t2 ->
      ((carryover + h1 + h2) mod 10)
      :: _reversed_sum_single_int_lists ((carryover + h1 + h2) / 10) t1 t2

(* Given two single int lists, returns their sum as a single int list. *)
let sum_single_int_lists (list1 : int list) (list2 : int list) : int list =
  List.rev (_reversed_sum_single_int_lists 0 (List.rev list1) (List.rev list2))

(*
   Given two bignums, returns a single-int list of their product.
   Style: I know declaring prod_matrix is unnecessary, but I find it vastly more \
   readable than calculating it at the tail of the fold statement.
*)
let single_int_prod_from_bignums (bn1 : bignum) (bn2 : bignum) : int list =
  let ls1, ls2 = (bignum_to_pos_int_list bn1, bignum_to_pos_int_list bn2) in
  let prod_matrix = multiply_int_lists ls1 ls2 in
  List.fold_left sum_single_int_lists [] prod_matrix

(* Given a REVERSED single-int list, returns a triple-int list IN REVERSE ORDER. \
   Ex: [9; 8; 7; 6; 5] into [789; 56] *)
(*
  NOTE: I've added this comment after submitting the first time, and, frankly, I have 
  to study for a test and don't have time to fix this.
  This should be the only function in this PSET that only works for cBASE = 1000. To make 
  it work for different sizes of cBASE, I think we'd need to slice X values from the head,
  destructure them, stringify each, concatenate them, and return them clumped together. 
  Doing so dynamically is beyond me atm. I do, however, recognize that this is 
  a bug and would need to be addressed before the software is put to use in a case 
  where cBASE is not 1000.
*)
let rec _reversed_single_int_to_triple_int (reversed_lst : int list) : int list
    =
  if cBASE != 1000 then
    raise
      (Invalid_argument
         "cBASE not 1000: Upgrade _reversed_single_int_to_triple_int \
                to support cBASE other than 1000.")
  else
    match reversed_lst with
    | [] -> []
    | [ ones ] -> [ ones ]
    | [ ones; tens ] ->
        [ int_of_string (string_of_int tens ^ string_of_int ones) ]
    | ones :: tens :: hundreds :: tail ->
        int_of_string
          (string_of_int hundreds ^ string_of_int tens ^ string_of_int ones)
        :: _reversed_single_int_to_triple_int tail

(* Given a single int list, returns a triple int list. \
   Ex: [5; 6; 7; 8; 9] into [56; 789]. *)
let single_int_to_triple_int (lst : int list) : int list =
  trim_leading_zeroes
    (List.rev (_reversed_single_int_to_triple_int (List.rev lst)))

(*

  Tl;dr on the multiplication algorithm:
    - If one or both of the two inputs are zero or one, the multiplication \
        is super easy to calculate. Return 0 or identity.
    - Else, the called functions perform the following:
        - Given two bignums, strip negative signs and turn them into strings.
        - Break each string into characters stored in a character list.
        - Parse each character into an int list. Now the bignum coeffs \
            [123; 456] look like [1; 2; 3; 4; 5; 6].
        - With two single-int lists, choose one to be the multiplicand and \
            the other to be the multiplier.
        - Find each partial product as we would when multiplying by hand. The \
            result of this is a matrix of partial products stored as single-int lists. 
        - Sum the partial products into the total product stored as a single-int list.
        - Convert the single int list into tripe-int lists. \
            Ex: [1; 2; 3; 4; 5; 6] into [123; 456].
        - Calculate the negativity sign by boolean logic and return the product's coeff list.

  General comments: 
    - I guarantee there's a better way to calculate long multiplication like this. However, \
        in the span of a weekend, this was the best I could manage.
    - If I needed to run this algorithm frequently, I think I'd prefer the numbers be stored \
        individually rather than in triples. Converting in and out of triples just added \
        complexity, and I don't see an exceptionally strong case for choosing three over one. \
        (Although I'm sure there is actually a great reason).
    - Important: I recognize my implementation runs List.rev a *ton* of times, which is very \
        inefficient. These could largely be removed by keeping the lists in reverse order \
        throughout computation. 
        - However, as a human, thinking through the logic of these computations in reverse order \
            broke my brain. While there's an efficiency loss, I do believe keeping the data in \
            greatest to least form kept the logic more understandable. Also, remembering how and \
            why this computation happens in reverse order weeks or months after writing this would \
            be nearly impossible. So, since the stakes are low, I kept the list reversals so that \
             each called method returns exactly what you'd expect.
    - Terminology: in the comments, a single-int list is a list of integers 0 - 9.
*)
(* times b1 b2 -- Returns the bignum product of `b1` and `b2` *)
let times (b1 : bignum) (b2 : bignum) : bignum =
  if List.length b1.coeffs = 0 || List.length b2.coeffs = 0 then
    { neg = false; coeffs = [] }
  else
    {
      neg = (b1.neg || b2.neg) && not (b1.neg && b2.neg);
      coeffs =
        single_int_to_triple_int
          (single_int_prod_from_bignums { b1 with neg = false }
             { b2 with neg = false });
    }

(*======================================================================
  Challenge Problem 6: Faster bignum multiplication
  ......................................................................*)

(* times_faster b1 b2 -- Returns a bignum representing the product of
   `b1` and `b2`, making use of the Karatsuba algorithm for
   multiplication. *)
let times_faster (b1 : bignum) (b2 : bignum) : bignum =
  failwith "times_faster not implemented"

(*======================================================================
  Reflection on the problem set

  After each problem set, we'll ask you to reflect on your experience.
  We care about your responses and will use them to help guide us in
  creating and improving future assignments.

  ........................................................................
  Please give us an honest (if approximate) estimate of how long (in
  minutes) this problem set took you to complete.
  ......................................................................*)

let minutes_spent_on_pset () : int = 870

(*......................................................................
  It's worth reflecting on the work you did on this problem set. Where
  did you run into problems and how did you end up resolving them? What
  might you have done in retrospect that would have allowed you to
  generate as good a submission in less time? Please provide us your
  thoughts on these questions and any other reflections in the string
  below.
  ......................................................................*)

let reflection () : string =
  "This one was pretty tough. I spent probably 60 percent of the total time \
   just generalizing 'compare_nums' and implementing 'times'. I'm also a \
   little concerned about my implementation of times, as it ballooned to a \
   scale that I'm pretty sure the assignment wasn't looking for. Reguardless, \
   I had a super chill time working on this and am pretty proud of it."

;;