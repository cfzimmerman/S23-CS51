(* 

References

*)

let remember = 
  let rem = ref "" in 
  fun (text : string) : string ->
      let temp = !rem in
      rem := text;
      temp

(*

Exercise 1

1 let p = ref 11 ;; (int ref)
2 let r = ref p ;; (int ref ref)
3 let s = ref !r ;; (int ref ref)
4 let t =
5 !s := 14;
6 !p + !(!r) + !(!s) ;;
7 let t =
8 s := ref 17;
9 !p + !(!r) + !(!s) ;;

1) After line 1, what is the type of p
int ref

2) After line 2, what is the type of r
int ref ref

3) After line 3, which of the following statements are true:

  (a) p and s have the same type 
  false

  (b) r and s have the same type 
  true

  (c) p and s have the same value (in the sense that p = s would be true)
  false

  (d) r and s have the same value (in the sense that r = s would be true)
  true

(4) After line 6, what is the value of t?
  42

(5) After line 9, what is the value of t?
  45

*)
  

(* 


Exercise 2: 

Write a function fib that generates the the next fibonacci number. The 

Fibonacci numbers are defined in the following way: 

F_0 = 0 

F_1 = 1 

F_n = F_{n - 1} - F_{n - 2} for n >= 2

*)

let fibonacci =
  let fib_older = ref 0 and 
  fib_younger = ref 1 in 
  fun () : int -> 
    let temp = !fib_older in 
    fib_older := !fib_younger;
    fib_younger := !fib_younger + temp;
    temp


type 'a mlist = 'a mlist_internal ref
 and 'a mlist_internal = 
   | Nil
   | Cons of 'a * 'a mlist ;;

(* 

Exercise 2: Mutable Lists 

Define a function mfirst : int -> ’a mlist -> ’a list that returns a list (immutable) 

of the first n elements of a mutable list:
*)


(*

Exercise 3: 

Write code to define a mutable integer list alternating such that for all integers n, the
expression mfirst n alternating returns a list of alternating 1s and 2s, for example,

# mfirst 5 alternating ;;
- : int list = [1; 2; 1; 2; 1]
# mfirst 8 alternating ;;
- : int list = [1; 2; 1; 2; 1; 2; 1; 2]

*)


