(*.....................

Binary Trees

.....................*)

type 'a bintree = 
| Empty 
| Node of 'a * 'a bintree * 'a bintree ;;



(* 

Exercise 0: 

Write a function called zip_trees, which zips two binary trees (similar to the zip function for lists), 

and raises an exception if two trees are of unequal length

*)


(*.....................

Exercise 1: 

Write a function sum_bintree: int bintree -> int

bintree that returns a the value of the sum of the nodes in a bintree of integers. 

# let int_bintree =
  Node (16,
  Node (93, Empty, Empty),
  Node (3,
  Node (42, Empty, Empty),
 Empty)) ;;
val int_bintree : int bintree =
Node (16, Node (93, Empty, Empty),
Node (3, Node (42, Empty, Empty), Empty)) 


# sum_bintree int_bintree ;;
- : int list = 154

.....................*)

let rec sum_bintree (tree : 'a bintree) : int = 
  match tree with
  | Empty -> 0
  | Node (value, left, right) -> value + sum_bintree left + sum_bintree right
;;


(*.....................

Exercise 2: 

Write a function preorder: 'a -> 'a bintree -> 'a list

bintree that returns a list of all the values stored in a tree in preorder; 

placing values stored at the node before values in the left subtree, 

and in turn before the values of the right subtree

# let int_bintree =
  Node (16,
  Node (93, Empty, Empty),
  Node (3,
  Node (42, Empty, Empty),
 Empty)) ;;
val int_bintree : int bintree =
Node (16, Node (93, Empty, Empty),
Node (3, Node (42, Empty, Empty), Empty)) 


# preorder int_bintree ;;
- : int list = [16; 93; 3; 42]

.....................*)

let rec preorder (tree : 'a bintree) : 'a list =
  match tree with
  | Empty -> []
  | Node (value, left, right) -> value :: preorder left @ preorder right
;;


(* 

Exercise 3: 

See similarities between the sum_bintree and preorder functions? Write a function called 

foldbt that walks over binary trees in a similar manner to the preorder and 

sum_bintree functions. The function takes in three arguments 

(1) the value to use for the empty trees 

(2) the function to apply to the values at each node in the walk and the 

values associated with the two subtrees 

(3) The tree to walk 

After implementing foltdt, reimplement the sum_bintree and preorder functions 

Take in the arguments in a manner analagous to fold_left on lists. 

*)

let rec foldbt (f : 'a -> 'b -> 'b -> 'b) (init : 'b) (tree : 'a bintree) : 'b =  
  match tree with
  | Empty -> init
  | Node (value, left, right) -> 
      f value (foldbt f init left) (foldbt f init right)
;;

let sum_bintree (tree : 'a bintree) : int =
  foldbt (fun value left right -> value + left + right) 0 tree
;;

let preorder (tree : 'a bintree) : 'a list =
  foldbt (fun x y z -> x :: y @ z) [] tree
;;
(*........................ 


Exercise 4: 

Define a function find : ’a bintree -> ’a -> bool in terms of foldbt, such that
find t v is true just in case the value v is found somewhere in the tree t.



.......................*)


(*.................

Exercise 5: 

An internal node of a binary tree has either one or two non-empty subtres. 

Write a function internals to collect them in a list.



.................*)


(*.................

Exercise 6: 

A leaf is a node with no successors. Write a function leaves to collect them in a list.


.................*)


(*....................... 

A node of a binary tree is at level N if the path from the root to the node has length N-1. 

The root node is at level 1. 

Write a function at_level t l to collect all nodes of the tree t at level l in a list.


.......................*)



    

(*......................

Exercise 7:

Define a type expr that represents all 

possible expressions of integers: an int, addition, subtraction, multiplication, division, and negation


....................*)


(*......................

Exercise 8:

Define a function e2s : expr -> string that returns a string that represents the 

fully parenthesized concrete syntax for the argument expression.

# e2s (Times (Plus (Int 3, Int 4), Neg (Int 5))) ;;
- : string = "((3 + 4) * (~- 5))"
# e2s (Int 42) ;;
- : string = "42"
# e2s (Div (Int 5, Int 0)) ;;
- : string = "(5 / 0)"

....................*)