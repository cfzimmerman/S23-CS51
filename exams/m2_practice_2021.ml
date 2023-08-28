(* Assumed available by questions *)
type 'a stream_internal = Cons of 'a * 'a stream
 and 'a stream = 'a stream_internal Lazy.t ;;

let head (s : 'a stream) : 'a =
  let Cons (hd, _tl) = Lazy.force s in hd ;;

let tail (s : 'a stream) : 'a stream =
  let Cons (_hd, tl) = Lazy.force s in tl ;;
  
let rec first (n : int) (s : 'a stream) : 'a list =
  if n = 0 then []
  else head s :: first (n - 1) (tail s) ;;

let rec smap (f : 'a -> 'b)
             (s : 'a stream)
           : 'b stream =
         lazy (Cons(f (head s), smap f (tail s)))
;;

let rec naturals = lazy (Cons(1, smap succ naturals)) ;;

let rec smap2 (f : 'a -> 'b -> 'c)
              (s1 : 'a stream)
              (s2 : 'b stream)
            : 'c stream = 
  lazy (Cons(f (head s1) (head s2), smap2 f (tail s1) (tail s2))) ;;

let rec sfilter (pred : 'a -> bool) (s : 'a stream) : 'a stream =
  let Cons(hd, tl) = Lazy.force s in 
  if pred hd then 
    lazy (Cons(hd, sfilter pred tl))
  else  
     sfilter pred tl
;;

(* Q2.1 *)

let nats_gen : unit -> int =
  let ct = ref ~-1 in 
  fun (): int -> 
    incr ct;
    !ct
  
(* Q2.2 *)
(* No, it cannot. Nats gen is definitionally impure. It generates different 
   values for identical inputs. 
   Since it must remember the previous number to generate the next, it must have 
   access to mutable state. *)

(* Q2.3 *)
let square_gen : unit -> int =
  let ct = ref 0 in 
  fun (): int -> 
    incr ct;
    !ct * !ct
  
(* Q2.4 *)
let rec stream_of_gen (gen : unit -> 'a) : 'a stream = 
  lazy (Cons(gen(), stream_of_gen gen )) ;;


exception NoMore ;;
(* Q2.5 *)
let gen_of_list (l : 'a list) : unit -> 'a =
  let store = ref l in 
  fun () : 'a -> 
    match !store with 
    | [] -> raise NoMore
    | hd :: tl -> store := tl; hd
;;

(* Q3.1 *)

(*
let f = fun x -> x + y in f x
free: x y
*)

(*
(fun x -> x + 1) 3
free: none
*)

(*
let f = fun x -> fun y -> if x < y then y else f y x in f 0 1
free: f
*)

(*
(fun x -> x) (fun y -> x)
free: x   
*)

(*
(fun x -> (fun y -> x) x)
free: none   
*)

(* Q3.2 *)
(*
   (fun y -> x)[x ↦ 42]
   fun y -> 42 

   ((fun y -> x) 21)[x ↦ 42]
   (fun y -> 42) 21

   (let x = x + 1 in x + 2)[x ↦ 42]
   let x = 42 + 1 in x + 2

   (let x = 5 in f y)[y ↦ x + 1]
   let z = 5 in f (x + 1)
*)

(* Q3.3 *)
(*
e: 3
f: {l_1 -> 3}
i: 42
j: {l_1 -> 3}
k: () 
l: {l_1 -> 42}

*)

(* Q3.4 *)
(*
let x = ref 3 in x := !x * 14 ;;
*)

(* Q4.2 *)
(* For practical analysis, big O notation is most relevant as the input limit 
   approaches infinity. Because big-O notation drops every term except the one
   that grows fastest at the limit, there be very substantial constant terms 
   dropped in the calculation. If elementary multiplication requires fewer total
   machine steps when the calculation implicates fewer than 71 digits, it might make
   sense to only use Karatsuba's algorithm when the 'n' in question is very large. *)

(* Q4.2 *)
(* 
Big-O relationship between F and G if F / G < infinity   

If the algorithm is O(n^2), it has a big-O relationship with the following:
1: no
2: O(n^2) 
3: no
4: O(n^3)
5: O(n^2 + cn)
6: O(2^n)
7: no
8: O(cn^2)
9: O(n^2 - c)
10: no
11: O(n^2 + c)
12: no

*)

(* Q6 *)
class type poke_type =
  object
  (* get_successor -- Returns the Pokemon that this one evolved
  into (as an option), or `None` if it hasn't (yet) evolved. *)
  method get_successor : poke_type option
  (* set_successor evolved_to -- Sets the successor Pokemon that
  this one `evolved_to`. *)
  method set_successor : poke_type option -> unit
  (* hit penalty -- Updates the hit points by reducing them by
  `penalty`. *)
  method hit : int -> unit
  (* is_dead -- Returns `true` if and only if this Pokemon is
  dead. *)
  method is_dead : bool
  (* evolve -- Evolves the Pokemon to a new one of whatever species
  it evolves to. *)
  method evolve : unit
  (* describe -- Returns a text description of the Pokemon and its
  evolutionary successors. *)
  method describe : string
 end

class pokemon (name : string) (init_hp : int) : poke_type =
  object
    val name : string = name

    val mutable hp : int = init_hp
    
    val mutable successor : poke_type option = None

    method get_successor = successor

    method set_successor evolved_to =
      successor <- evolved_to

    method hit penalty =
      hp <- max 0 (hp - penalty)

    method is_dead =
      hp <= 0

    method evolve =
      hp <- 0

    method describe =
      Printf.sprintf "%s [%d] --> %s"
      name hp (match successor with
      | None -> ""
      | Some next -> next#describe)
 end

 class raichu =
  object
    inherit pokemon "Raichu" 122
  end

  class pikachu =
    object (self)
      inherit pokemon "Pikachu" 82 as super
      method! evolve = 
        super#evolve;
        super#set_successor (Some(new raichu))
  end