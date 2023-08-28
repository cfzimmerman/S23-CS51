let filter = List.filter
let map = List.map
let fold_left = List.fold_left
let fold_right = List.fold_right

let lst = [1; 2; 3; 4; 5]

let filter_odds= filter (fun el -> el mod 2 = 0) lst
(*
  returns int list = [2; 4]
*)

let map_incr_5 = map ((+) 5) lst
(* 
  returns  int list = [6; 7; 8; 9; 10] 
  let () = List.iter (Printf.printf "%d ") map_example
*)

let fold_left_reverse = fold_left (fun acc el -> el :: acc) [] lst 
  (* returns int list = [5; 4; 3; 2; 1] *)

let fold_right_reverse = fold_right (fun el acc -> acc @ [el]) lst [] 
(* returns int list = [5; 4; 3; 2; 1] *)

(* SIMPLE STACK *)
module type STACK =
  sig
    type elt
    type stack
    exception Empty of string
    val empty : unit -> stack
    val push : stack * elt -> stack
    val pop : stack -> elt * stack
    val isEmpty : stack -> bool
  end

module MakeStack (Arg : sig type t end) : (STACK with type elt = Arg.t) =
  struct 
    type elt = Arg.t 
    type stack = elt list
    exception Empty of string
    let empty () = []
    let push (st, el) = el :: st
    let pop st = match st with
      | hd :: tl -> (hd, tl)
      | [] -> raise (Empty "empty stack")
    let isEmpty = fun st -> st = []
  end

module IntStack : (STACK with type elt = int) = 
  MakeStack (struct type t = int end) ;;

IntStack.push (IntStack.empty (), 3)

