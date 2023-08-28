
open NativeLazyStreams ;;

(*

lab 10: Big O
lab 13: tail recursion
lab15 : streams
lab16 and 17: OOP

*)

(* Q1 *)
(*
1: int ref
2: unit
3: unit -> int
4: 44
5: 44
*)

(* Q2 *)
(*
6: 2
7: (||)   
8: (x := 0; false)
9: x > 50
10: x := 200; true
*)

(* Q3 *)
(*
4   
*)

(* Q4 *)
(*
 Function is called twice for each layer. Complexity is 2^n

  Alternate version:
  Complexity is O(n)
*)

(* Q14 *)
(*
Yes, passing n = 0, the first function returns 1 and the second returns 0.    
*)

(* 15 *)
let rec nats = lazy (Cons(0, smap succ nats)) ;;

let rec threeby: int stream = 
  lazy (Cons (3, smap ((+) 2) threeby)) ;;

  (* times 2 + 3 *)
let threeby2: int stream = 
  smap (fun x -> x * 2 + 3) nats ;;

(* Q16 *)
class type fish_type =
  object
    method get_level : int
    method get_weight : int
    method set_weight : int -> unit
  end ;;

class fish (initial_level : int) (initial_weight : int) : fish_type =
  object (self)
    val mutable level : int = initial_level
    val mutable weight : int = initial_weight
    
    method get_level : int = level
    method get_weight : int = weight
    method set_weight (new_w : int) : unit = 
      weight <- new_w
  end

class guppy =
  object 
    inherit fish 1 1
  end

class trout =
  object
    inherit fish 2 15
  end

let compare_fish (fish1 : fish_type) (fish2 : fish_type) : int =
    let comp = compare (fish1#get_weight) (fish2#get_weight) in 
    if comp > 0 then 1 else if comp < 0 then ~-1 else 0
  
let eat_fish (eating_fish : fish_type) (eaten_fish : fish_type) : unit =
    eating_fish#set_weight (eating_fish#get_weight + eaten_fish#get_weight);
    eaten_fish#set_weight 0

let attack (fish1 : fish_type) (fish2 : fish_type) : unit =
    match compare_fish fish1 fish2 with 
    | 0 -> ()
    | 1 -> eat_fish fish1 fish2
    | -1 -> eat_fish fish2 fish1
    | _ -> failwith "error: compare_fish returned unexpected value"

(*
  let a_guppy = new guppy ;;
  let a_trout = new trout ;;
  a_guppy#get_level ;;
  a_trout#get_weight ;;
  a_trout#set_weight 15;;
  a_trout#get_weight ;;
*)

(* Problem 18 *)
(*

{x -> l1; y -> l1}, {l1 → 1} ⊢ x := 3; !y 
    | x := 3 (eval)  {x -> l1; y -> l1}, {l1 → 1}
        | x (eval) l1 {x -> l1; y -> l1}, {l1 → 1}
        | 3 (eval) 3
        unit {x -> l1; y -> l1}, {l1 → 3}
        | !y {x -> l1; y -> l1}, {l1 → 3}
        | 3 {x -> l1; y -> l1}, {l1 → 3}
    | 3 {x -> l1; y -> l1}, {l1 → 3}
*)

(*
  let x = ref 1 in let y = x in x := 3; !y ;;   
*)