(*
                          CS 51 Problem Set
                 Simulation of an Infectious Disease

                     Increment/Decrement Counters
 *)

class type counter_type =
  object
    (* set n -- Sets the running count to `n`. *)
    method set : int -> unit

    (* reset -- Resets the running count to zero. *)
    method reset : unit

    (* bump -- Increments the count. *)
    method bump : unit

    (* debump -- Decrements the count. *)
    method debump : unit

    (* count -- Returns the current count, initially zero. *)
    method count : int
  end

(*....................................................................
  Place your implementation of the `counter` class of class type
  `counter_type` here.
  ....................................................................*)

class counter : counter_type =
  object (this)
    val mutable count = 0

    method set (num : int) : unit = count <- num

    method reset : unit = this#set 0

    method bump : unit = this#set (count + 1)

    method debump : unit = this#set (count - 1)

    method count : int = count
  end

(* Now you should be able to test the partially implemented simulation
   with

         % ocamlbuild run.byte
         % ./run.byte

   Once that is working, you should extend the simulation in the file
   `people.ml`.

   ......................................................................
   When you've completed the assignment and made a video demonstrating
   your simulation, define the function `recording_url` below to return
   the URL where the recording can be found.
   ....................................................................*)

(*
   
🚨 Comments for the grader:

Due to the line "If they involve extensions of
the sort above, all the better" in the PDF readme, I assumed simulation extensions
were optional.

However, during lab 19, Professor Schieber seemed to imply that we should add
extensions to the simulations.

I had already filmed the video and submitted the problem set, but, in the event
extensions are required, I did add one critical modification. My reflections on it are 
provided here instead of re-filming the whole video, which I still believe fulfills 
what is requested of problem 6. 

EXTENSION:
I thought it would be interesting to calculate mortality at every time step rather than 
just at the end of the infection period. That would of-course allow more mortality events, 
but it could also meaningfully decrease the number of infected people at any given time. 
I was curious to see how these effects would balance. The modification can be found in 
people.ml / (class) infected.

FINDINGS: 
I substantially decreased the mortality coefficient to fairly reflect the increased 
number of mortality-possible events. In the simulation at cMORTALITY=0.1, however, the 
effect of increased fatalities far outweighed any benefits gained from the decreased 
number of infected people at any given time. 

REFLECTION:
A small change in the structure of the simulation (when we're calculating 
a mortality event) had a huge impact on the number of overall fatalities 
in the simulation. To me, that's a powerful reminder that we need to 
be really careful when modeling real-world situations in code, as small 
simplifications for the sake of computation can seriously skew our results.

*)
let recording_url () : string = "https://youtu.be/LsGhek77wbM"

(*====================================================================
  Reflection on the problem set

  After each problem set, we'll ask you to reflect on your experience.
  We care about your responses and will use them to help guide us in
  creating and improving future assignments.

  ......................................................................
  Please give us an honest (if approximate) estimate of how long (in
  minutes) this problem set (in total, not just this file) took you to
  complete.
  ....................................................................*)

let minutes_spent_on_pset () : int = 300

(*....................................................................
  It's worth reflecting on the work you did on this problem set, where
  you ran into problems and how you ended up resolving them. What might
  you have done in retrospect that would have allowed you to generate as
  good a submission in less time? Please provide us your thoughts in the
  string below.
  ....................................................................*)

let reflection () : string = "OOP in OCaml is lots of fun. In this \
PSET, I liked how we got to use objects alongside modules to get a \
sense of where they each performed best. The modeling was cool too."
