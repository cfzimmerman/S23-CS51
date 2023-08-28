(*
                         CS 51 Problem Set 7
                       Refs, Streams, and Music
                   Part 1: Mutable Lists and Cycles
 *)
(* The type of mutable lists. *)
type 'a mlist = 'a mlist_internal ref

and 'a mlist_internal = Nil | Cons of 'a * 'a mlist

(*......................................................................
  Problem 1: Write a function `has_cycle` that returns `true` if a
  mutable list has a cycle, `false` otherwise. You may want a recursive
  auxiliary function. You needn't worry about space usage of your
  function.

  For instance, we can establish a cyclic and an acyclic mutable list
  like this:

      # let sample_end = ref Nil ;;
      # let cyclic = ref (Cons (1, ref (Cons (2, sample_end)))) ;;
      # sample_end := !cyclic ;;
      # let acyclic = ref (Cons (3, ref (Cons(4, ref Nil)))) ;;

  and test for cycles using `has_cycle`:

      # has_cycle cyclic ;;
      - : bool = true
      # has_cycle acyclic ;;
      - : bool = false
  ......................................................................*)

let has_cycle (lst : 'a mlist) : bool =
  let rec traverse_list (fast : 'a mlist) (slow : 'a mlist) : bool =
    match (!fast, !slow) with
    | Nil, _ -> false
    | Cons (_, ft), Cons (_, st) -> (
        if fast == slow then true
        else
          match !ft with Nil -> false | Cons (_, ft2) -> traverse_list ft2 st)
    | _ ->
        failwith
          "Logic error: slow should never reach the end of the list first"
  in
  match !lst with Nil -> false | Cons (_, tl) -> traverse_list tl lst

(*......................................................................
  Problem 2: Write a function `flatten` that flattens a list (removes
  its cycles if it has any) destructively. Again, you may want a
  recursive auxiliary function, and you shouldn't worry about space.
  ......................................................................*)

(* Design note: mlist_iterator would be much more efficient using a Set or Hash
   data structure to store 'seen' than a list. However due to the lack of dynamic
   polymorphism in Set, integration with the given signature for flatten was near
   impossible (I think we'd need a compare function and type value to pass in as a functor
   to Set). Hashtbl was prone to bugs, so I'm leaving seen as a less-efficient list. *)
let rec mlist_iterator (lst : 'a mlist) (seen : 'a mlist list)
    (on_end_reached : 'a mlist -> 'a mlist list -> int) : int =
  match !lst with
  | Nil -> on_end_reached lst seen
  | Cons (_, tl) ->
      if List.length (List.filter (( == ) tl) seen) > 0 then
        on_end_reached lst seen
      else mlist_iterator tl (lst :: seen) on_end_reached

let flatten (l : 'a mlist) : unit =
  let _ =
    mlist_iterator l [] (fun lst _ ->
        lst := Nil;
        0)
  in
  ()

(*......................................................................
  Problem 3: Write a function `mlength`, which nondestructively returns
  the number of nodes (that is `Cons`es) in a mutable list that may have
  cycles.
  ......................................................................*)
let mlength (lst : 'a mlist) : int =
  mlist_iterator lst [] (fun _ seen -> List.length seen)

(*======================================================================
  Reflection on the problem set

  After each problem set, we'll ask you to reflect on your experience.
  We care about your responses and will use them to help guide us in
  creating and improving future assignments.

  ........................................................................
  Please give us an honest (if approximate) estimate of how long (in
  minutes) this problem set (in total, not just this file) took you to
  complete. (If you worked with a partner, we're asking for how much time
  each of you (on average) spent on the problem set, not in total.)
  ......................................................................*)

let minutes_spent_on_pset () : int = 720

(*......................................................................
  It's worth reflecting on the work you did on this problem set, where
  you ran into problems and how you ended up resolving them. What might
  you have done in retrospect that would have allowed you to generate as
  good a submission in less time? Please provide us your thoughts in the
  string below.
  ......................................................................*)

let reflection () : string =
  "Another fun Pset. Infinite trees were kind of mind boggling, and the \
   leetcode-style practice in refs was appreciated. Overall, I think we got \
   the right answers for music and Canon (sounds the same as canon_soln), but \
   tbh that doesn't sound like the Canon I was thinking of 😅."
