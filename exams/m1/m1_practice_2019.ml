(* Q1.1 *)
let f b = if b then () else ()

(* Q1.2 *)
let io num = Some(num + 1)

(* Q1.3 *)
let io (poly, bl) = if bl then poly else poly

(* Q1.4 *)
let io (poly) = Some([])

(* Q1.4 *)
let rec asdf (left) (right) =
  asdf left right

(* Q2.1 
f : float -> float 
*)

(* Q2.2
f : 'a -> 'a
*)

(* Q2.3
f: (int * int * int)
*)

(* Q2.4
f: Ill formed
*)

(* Q2.5
f: 'a -> 'a list -> 'a list
*)

(* Q3.1
bool -> string
*)

(* Q3.2 *)

let s_of_b (bl : bool) : string =
  match bl with
  | true -> "true"
  | false -> "false"
;; 

(* Q5.1 
   First, compare_lengths has O(n) time complexity
   beause OCaml doesn't store the length of the list
   as an attribute of the list. So, to calculate the 
   length of a list, it needs to traverse the length
   of the list and count how many elements it sees. 
   For a list of length n, that's O(n) time complexity. 

   The complexity is generally the length of the smaller
   list plus the length of the longer list. Because we drop
   scalars and constants from the computation of time
   complexity, we'll just choose the worst case of O(n), which
   is where n is the length of the longest list.
*)

(* Q6.1 *)
let rec compare_lengths (l1 : 'a list) (l2 : 'b list) : int =
  match l1, l2 with
  | [], [] -> 0 
  | [], _ -> ~-1
  | _, [] -> 1
  | _ :: t1, _ :: t2 -> compare_lengths t1 t2
;;

(* Q7.1

(fun x -> x + 2) 3
  (fun x -> x + 2) ⇓ (fun x -> x + 2)
  3 ⇓ 3
  (fun x -> x + 2) [x → 3]
    (x + 2) [x → 3]
      3 + 2
        3 ⇓ 3
        2 ⇓ 2
        5
    5
*)

(* Q8.1
  1. Used recursively, this needs a let rec
  1. Type needs to be 'a bintree, not just bintree, also needs to be typed, 'a bintree
  2. Need to match tree, not bintree
  5. This places the item on the left of old if item is greater. This doesn't satisfy the given invariant of a sorted binary tree.
  8. Only providing two terms in a triple. Need to add a comma somewhere. Again, node requires three elements.
*)

(* Q9 *)
type 'a bintree = 
  | Empty 
  | Node of 'a bintree * 'a * 'a bintree

type direction = Left | Right

let rec gorn (target : 'a) (tree : 'a bintree) : direction list =
  match tree with 
  | Empty -> raise (Failure "not found")
  | Node(left, value, right) ->
    if value = target then []
    else if value < target then Left :: (gorn target left)
    else Right :: (gorn target right) ;;

let rec find (target : 'a) (tree : 'a bintree) : bool =
  match tree with 
  | Empty -> false 
  | Node(left, value, right) -> 
    if value = target then true 
    else if value < target then find target left
    else find target right
;;

(* Alt *)
let find (target : 'a) (tree : 'a bintree) : bool =
  try
    let _ = gorn target tree in true
  with
  | Failure _ -> false