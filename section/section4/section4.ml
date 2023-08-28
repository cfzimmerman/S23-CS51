(*     Code Review 4  

We will be implementing the Set Module from scratch. Sets are an unordered list 
with no duplicate elements. 

*)

(* 

Part 1: Define a module signature for an ORDERED_TYPE which specifies the type of 
the elements, a function to compare elements, and a function to convert each of the elements into strings. 

*)

module type SET_FUNCTOR =
  sig
    type t
    val compare : t -> t -> int
    val to_string : t -> string
  end
;;

(* 
Part 2: 

Create a module signature for the Set Module Type: 

We want the following operations /values on sets to be supportable: 

1) empty : the empty set

2) add : Add an element to a set 

3) Given an eleemnt, remove it from a set, 
  and return the set containing the rest of the elements 

4) mem : check if an eleement is a member of a set

5) union : return the union of two sets 

6) intersection : return the intersection of two sets 

7) print_set : convert a set into a string : You would represent a set 
representing the integers 1, 5, 6, 7, 9 as {1, 5, 6, 7, 9}

8) take : a function that takes an element in the set and returns the rest of the 
elements in the set as a pair : (h, t), where h is the extract element and t are the rest 
of the elements.

*)

module type CUSTOM_SET =
  sig
    type element
    type set
    val empty : set
    val add : set -> element -> set
        (*
    val rem : element -> set
    val mem : set -> element -> bool
    val union : set -> set -> set
    val intersection : set -> set -> set
    val print_set : set -> string
    val take : set -> element * set
    *)
  end
;;

module Custom_Set (Inputs : SET_FUNCTOR) : CUSTOM_SET = 
  struct
    type element = Inputs.t
    (* type set = (element, bool) Hashtbl.t *)
    type set = element list
    
    let empty = []
    let rec add (set : set) (el : element) : set = 
      match set with 
      | [] -> [el]
      | value :: tail -> 
          if value = el then value :: tail else
          add tail el
          (* Call list.mem instead! *)
  end
;;


module type INT_SET = SET_FUNCTOR with type t = int ;;

(* 
Part 3: 
Use the MakeSet to make the following modules 
1) A set of ints 

2) A set of strings 

3) A set of lists 

4) A set of integer sets
*)


(* 

Exercise: 

Define a function power_set that returns a set of all subsets of the original set. 
You can just make a function that returns the power set for a set of integers


*)






