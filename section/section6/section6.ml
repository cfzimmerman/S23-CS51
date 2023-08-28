open NativeLazyStreams ;;

(* 

Thunks 

*)

module type THUNK = 
  sig 
    type 'a t 
    val lazy_func: 'a -> 'a t 
    val force : 'a t -> 'a

  end

(* 

Exercise 1: 

Define a module of type THUNK. 

1) lazy_func -> Given a expression, should return a thunk holding the value 

2) Force -> Should force computation of the expression inside a thunk 

*)

module Thunk : THUNK = 
  struct 

    type 'a t = 'a t_internal ref and 
      'a t_internal =  
      | Unevaluated of (unit -> 'a)
      | Evaluated of 'a 

    let lazy_func (e : 'a) : 'a t = 
      ref (Unevaluated(
          fun () -> e
        ))

    let rec force (thunk : 'a t) : 'a = 
      match !thunk with 
      | Evaluated el -> el
      | Unevaluated f -> 
          thunk := Evaluated(f());
          force thunk
  end



(* 

Exercise 2: Laziness 

Define a function (&&&) : bool Lazy.t -> bool Lazy.t -> bool. 

It should behave like a short circuit Boolean AND. 

That is, lb1 &&& lb2 should first force lb1. 

If the value is false, the function should return false. 

Otherwise, it should force lb2 and return its value.

*)

let (&&&) (x : bool Lazy.t) (y : bool Lazy.t) : bool =
  Lazy.force x && Lazy.force y

(* 

Streams

*)


(* 

Exercise 3: 

Define a value pow2 : int stream whose elements are the powers of two:

*)
      
let rec pow2: int stream = lazy (Cons(1, smap (( * ) 2) pow2))

(* 

Exercise 4: Define a stream whose elements are the lowercase letters of the 

alphabet on repeat: a, b, c ....., z, a, b, c ..., z ...

Hint : The chr and code function in OCaml's Char module might be helpful 

*)

let next_char (c : char) : char = 
  let cd = Char.code c in
  if cd >= 122 then Char.chr 97 else Char.chr (cd + 1)


let rec lowercase: char stream = lazy (Cons('a', smap next_char lowercase))

(* 

Exercise 5: 

Define a function total : int stream -> int stream, 

such that total <a; b; c; ...> 

is a running total of the input elements, i.e., <a; a + b; a + b + c; ...>.

*)

let running_sum (st : int stream) : int stream =
  let rec helper (pred: int) (s : int stream) : int stream = 
    let lazy (Cons(hd, tl)) = s in 
    lazy (Cons(hd + pred, helper (hd + pred) tl))
  in 
  helper 0 st
;;


(* 

Exercise 6: 

Problem Section 17.5: Circuts and Boolean Streams 

(a) Define a value falses to be an infinite stream of the boolean value false.


(b) What is the type of falses?

(c) A useful function is the trueat function. The expression trueat n 

    generates a stream of values that are all false 
    
    except for a single true at index n:

# first 5 (trueat 1) ;;
- : bool list = [false; true; false; false; false]

Define the function trueat.

(d) Define a function circand to represent the and gate. It should have the following
behavior:

# first 5 (circand (circnot (trueat 1)) (circnot (trueat 3))) ;;
- : bool list = [true; false; true; false; true]




(e) Succinctly define a function circnand using the functions above to represent the nand
    gate. It should have the following behavior:
    # first 5 (circnand falses (trueat 3)) ;;
    
    - : bool list = [true; true; true; true; true]
      
      # first 5 (circnand (trueat 3) (trueat 3)) ;;
      
      - : bool list = [true; true; true; false; true]


*)


