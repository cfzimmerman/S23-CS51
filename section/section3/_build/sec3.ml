(*.....................

Binary Trees

.....................*)

type 'a bintree = 
| Empty 
| Node of 'a * 'a bintree * 'a bintree ;;

(*.....................

Exercise 1: 

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

let rec preorder (tree: 'a bintree): 'a list =  
  match tree with 
  | Empty -> []
  | Node (v, left, right) -> 
      v :: (preorder left @ preorder right)
 ;;
