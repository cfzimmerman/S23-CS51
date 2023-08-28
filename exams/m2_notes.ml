(*

ocamlbuild -use-ocamlfind m2_notes.ml 
#mod_use "nativeLazyStreams.ml";;
#use "m2_notes.ml" ;;

*)

open NativeLazyStreams ;;

(* Q2.1 *)
let flip_coin (probability : float) : bool =
  Random.float 1.0 < probability ;;

(* Q2.2 *)
type toggle = On | Off ;;

(* Q2.3 *)
let toggle (t : toggle) : toggle =
  match t with 
  | On -> Off
  | Off -> On
;;

(* Q2.4 *)
let toggle_name (t : toggle) : string =
  match t with 
  | On -> "on"
  | Off -> "off"
;;

(* Q3 *)
let rec fold_left_func (f : 'a -> 'b -> 'a) (init : 'a) (xs : 'b list) : 'a =
  match xs with
  | [] -> init
  | hd :: tl -> fold_left_func f (f init hd) tl ;;

(* Q3 - recheck this! *)
let fold_left (f : 'a -> 'b -> 'a) (init : 'a) (xs : 'b list) : 'a =
  let result = ref init in 
    for counter = 0 to ((List.length xs) - 1) do 
      result := f !result (List.nth xs counter)
    done;
    !result
;;

(* Q4 *)
let rec repeat (lst : 'a list) : 'a stream = 
  match lst with
  | [] -> raise (Failure "repeat: empty argument")
  | hd :: tl ->
     lazy (Cons (hd,
	    repeat (tl @ [hd]))) ;;


(* Q4.8 *)
let rec uniform_crossover (s1 : 'a stream) (s2 : 'a stream) : 'a stream =
  if flip_coin 0.5 then 
    lazy (Cons(head s1, uniform_crossover (tail s1) (tail s2)))
  else 
    lazy (Cons(head s2, uniform_crossover (tail s1) (tail s2))) ;;

(* Check this *)
let rec swap_crossover (prob : float) (s1 : 'a stream) (s2 : 'a stream) : 'a stream =
  if flip_coin prob then 
    lazy (Cons(head s2, swap_crossover prob (tail s2) (tail s1)))
  else 
    lazy (Cons(head s1, swap_crossover prob (tail s1) (tail s2))) ;;
   
(*

Q5

read_numbers: O(n) worst case
sort_numbers: O(n * log(n)) worst case

*)

(* semantics *)
(*




*)


(* Q8 *)
class widget (name : string) =
  object (this)
    method state_name = "(no state)"
    method get_name = name ^ ":" ^ this#state_name
  end ;;

class button (name : string) (initial_state : 'a) (state_name : 'a -> string) =
  object (this)
    inherit widget name
      val mutable state = initial_state
      method state_name = state_name state
      method click = ()
  end ;;

class button_updatable (name : string) (initial_state : 'a) (next_state : 'a -> 'a) (state_name : 'a -> string) =
  object (this)
    inherit button name initial_state state_name
    method !click = state <- (next_state state)
  end

class button_toggle (name : string) = 
  object (this)
    inherit button_updatable name Off toggle toggle_name
end;;

class button_counter (name: string) =
  object (this)
    inherit button_updatable name 0 (succ) (string_of_int) 
end ;;