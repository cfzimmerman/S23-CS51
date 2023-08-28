(* Q2.1 *)
let add_three = (+) 3 ;;

(* Q2.2 *)
let rec curried_power =
  fun (num : int) ->
    fun (pow : int) ->
      match (pow : int) with 
      | 0 -> 1
      | 1 -> num
      | _ -> num * curried_power num (pow - 1)

(* Q2.3 *)
let rec power (num, pow: int * int) : int =  curried_power num pow

type 'a combine =
  | Combine of ('a -> 'a) * ('a combine)
  | Base of ('a -> 'a) * ('a option) ;;

let rec f x a =
  match x with 
  | Base (f, None) -> f a
  | Base (f, Some x) -> f x
  | Combine (g, r) -> f r (g a) ;;

(* Q3.1 
   'a
*)

(* Q3.2
  'a -> 'a
*)

(* Q3.3
  'a combine -> 'a -> 'a
*)

(* Q3.4
  'a -> 'a
*)

(* Q3.5
  'a
*)

(* Q4.1
GOAL 42

[42]
   
*)

(* Q4.2
  No such expression. 
  TIME these.
*)

(* Q4.3
  let setup = (fun _ -> 42) in 
    setup (setup 21)
*)

(* Q4.4
  { first = 6; second = true } ;;
*)

(* Q5.1 *)
let tower (lst : int list) : int =
  List.fold_right (fun el acc -> power(el, acc)) lst 1 

(* Q5.2 *)
exception Not_found
let find (search : 'a -> bool) (lst : 'a list) = 
    match List.filter search lst with 
    | [] -> raise Not_found
    | hd :: _ -> hd

(* Q6.1 *)
type circuit = 
  | Single of float
  | Series of circuit * circuit 
  | Parallel of circuit * circuit
;;

(* Q6.2 *)
let circ_a = Single 3.

(* Q6.3 *)
let circ_c = Parallel(Single 2., Single 4.)

(* Q6.4 *)
let circ_d = Series((Parallel(Single 4., Parallel(Single 2., Single 4.))), (Single 1.))

(* Q6.5 *)
let rec resistance (cir : circuit) : float =
  match cir with 
  | Single(el) -> el
  | Series(fst, snd) -> (resistance fst) +. (resistance snd)
  | Parallel(fst, snd) -> 1. /. ((1. /. (resistance fst)) +. (1. /. (resistance snd)))
;;

(* Q7.1 *)
module type SEQUENCE =
  sig
  type t
  val sequence : int -> t list
  val print_sequence : t list -> unit
end

module Natnums : SEQUENCE =
  struct
    type t = int
    let rec sequence_from from length =
      if length = 0 then [] 
      else from :: sequence_from (succ from) (length - 1)
    let sequence length = sequence_from 0 length
    let print_sequence (lst : t list) = List.iter (fun el -> Printf.printf "%d" el) lst
  end

module Diminishing : SEQUENCE =
  struct
    type t = float 
    let rec sequence_from from length = 
      if length = 0 then [] 
      else from :: sequence_from (from /. 2.) (length - 1)
    let sequence length = sequence_from 1. length
    let print_sequence (lst : t list) = List.iter (fun el -> Printf.printf "%f" el) lst
  end

  module type ELEMENT =
    sig
      type t
      val initial : t
      val gen_next : t -> t
      val print : t -> unit
    end

module Sequence (Element : ELEMENT) : SEQUENCE = 
  struct
    type t = Element.t
    let rec sequence_from (from : t) (length : int) : t list =
      if length = 0 then [] 
      else from :: sequence_from (Element.gen_next from) (length - 1)
    let sequence (length : int) : t list = sequence_from Element.initial length
    let print_sequence (lst : t list) = List.iter (fun el -> Element.print el; print_string " ") lst
  end

module Natnums = Sequence (struct
  type t = int
  let initial = 0
  let gen_next = succ
  let print = print_int
end)

module Diminishing = Sequence (struct
  type t = float
  let initial = 1.
  let gen_next el : t = el /. 2.
  let print = print_float
end)
;;

Natnums.print_sequence (Natnums.sequence 5) ;;
Diminishing.print_sequence (Diminishing.sequence 5) ;;