(*
   
This file contains assorted logic for instantiating and testing our created modules.

Note, certain contents (as marked) were copied directly from the tests.ml file. The 
problem set specification recommended we not touch other files, but opening 
Tests here caused the GUI drawing to pop up. So, we opted to copy over necessary logic
and leave the rest. Please see our attached findings for synthesized information.

*)

open Collections
open Mazes
open Tiles
open Puzzlesolve
open Puzzledescription
open CS51Utils

(* Performance test of Queue implementations *)
module IntQueueList = MakeQueueList (struct
  type t = int
end)

module IntQueueStack = MakeQueueStack (struct
  type t = int
end)

module type BENCHMARK_QUEUE = sig
  val run_benchmark : unit -> float
end

module BenchmarkQueue (Collection : COLLECTION with type elt = int) :
  BENCHMARK_QUEUE = struct
  let min = 0
  let max = 200

  let rec enqueue_many (count : int) (q : Collection.collection) :
      Collection.collection =
    if count < min then q else enqueue_many (count - 1) (Collection.add count q)

  let rec dequeue_many (count : int) (q : Collection.collection) :
      Collection.collection =
    if count > max then q
    else
      let value, coll = Collection.take q in
      dequeue_many (count + 1) coll
  let run_benchmark () : float =
    let start = Unix.gettimeofday () in
    let coll = dequeue_many min (enqueue_many max Collection.empty) in
    let stop = Unix.gettimeofday () in
    assert (Collection.is_empty coll);
    stop -. start
end

module BenchmarkIntQueueList = BenchmarkQueue (IntQueueList)
module BenchmarkIntQueueStack = BenchmarkQueue (IntQueueStack)
;;

let narrate_queue_difference () =
  let ql = BenchmarkIntQueueList.run_benchmark ()
  and qs = BenchmarkIntQueueStack.run_benchmark () in
  Printf.printf
    "\n\
     List-based: %f seconds\n\
     Stack-based: %f seconds\n\
     💡 The stack-based queue enqueued and dequeued 20,000 integers %f seconds \
     faster than the list-based queue.\n\n"
    ql qs (ql -. qs) ;;

(* 🧠 Benchmarked at 50,000 integers, the stack-based queue outperformed \
   the list based queue by 0.003971 seconds to 37.952160 seconds *)
;;

narrate_queue_difference()  ;;

(* init_maze, square_maze, and copy_maze are taken from the tests file.
   Even importing the tests file caused drawing to occur, and I didn't want
   that to happen. *)
let init_maze = [|
    [| EmptySpace; EmptySpace; Wall; EmptySpace; EmptySpace|];
    [| Wall; EmptySpace; EmptySpace; EmptySpace; EmptySpace|];
    [| Wall; Wall; EmptySpace; Wall; EmptySpace|];
    [| EmptySpace; EmptySpace; EmptySpace; Wall; EmptySpace|];
    [| EmptySpace; Wall; EmptySpace; EmptySpace; EmptySpace|];
   |] ;;

  let square_maze (copies : int) : maze =
  let orig = 5 (* dimensions of original square maze *) in
  let new_maze = Array.make_matrix
                    (orig * copies) (orig * copies)
                    EmptySpace in
  let col_bound = (orig * copies) in 
  let row_bound = (orig * copies) - orig in
  
  (* copy_maze -- tile the original maze into the new maze *)
  let rec copy_maze (crow: int) (ccol: int) : maze =     
    if (ccol = col_bound && crow = row_bound) then new_maze
    else if (ccol = col_bound) then 
      copy_maze (crow + orig) 0
    else
      begin
        List.init orig Fun.id (* for each row *)
        |> List.iter (fun offset ->
                      Array.blit init_maze.((crow + offset) mod orig) 0
                                  new_maze.(crow + offset) ccol orig);
        (* keep on recurring *)
        copy_maze (crow) (ccol + orig)
      end in
  
  copy_maze 0 0 ;;


module BigMazePuzzle = MakeMazePuzzleDescription(  
  struct
    let maze = square_maze 3
    let initial_pos = (0, 0)
    let goal_pos = (14, 14)
    let dims = (15, 15)
  end) ;;

module BigMazeDFS = DFSSolver(BigMazePuzzle) ;;
module BigMazeBFS = BFSSolver(BigMazePuzzle) ;;
module BigMazeBFSFaster = FastBFSSolver(BigMazePuzzle) ;;

print_endline "\nMaze DFS: " ;;
Absbook.call_reporting_time BigMazeDFS.solve () ;;
print_endline "Maze List BFS: " ;;
Absbook.call_reporting_time BigMazeBFS.solve () ;;
print_endline "Maze Stack BFS: " ;;
Absbook.call_reporting_time BigMazeBFSFaster.solve() ;;

(* Again, random seed through random_tileboard was copied from tests.ml.
   If the PDF didn't ask us to leave non-experiments files alone, I'd be 
   very inclined to remove the draw-by-default setting. *)

let _  = Random.init 0 ;;  


(* 😳 All three searches on a 4x4 took more than 5 minutes (how much longer, unknown). *)

let cDIMS = 4, 4 ;;
let solved : board =
  [| [|Tile 1; Tile 2; Tile 3; Tile 9|];
     [|Tile 4; Tile 5; Tile 6; Tile 10|];
     [|Tile 7; Tile 8; Tile 11; Tile 12|];
     [| Tile 13; Tile 14; Tile 15; EmptyTile |] |] ;;


(* 
Alternate board sizes we've tested.

let cDIMS = 3, 3
let solved : board =
  [| [|Tile 1; Tile 2; Tile 3|];
     [|Tile 4; Tile 5; Tile 6|];
     [|Tile 7; Tile 8; EmptyTile|]; |] ;; 

let cDIMS = 2, 2
let solved : board =
  [| [|Tile 1; Tile 2|];
     [|Tile 3; EmptyTile|]; |] ;;

*)
                     
let rand_elt l : board = 
  fst (List.nth l (Random.int (List.length l))) ;;

let random_tileboard () : board =
  let cINITIAL_MOVE_COUNT = 45 in
  let module Puzzle : (PUZZLEDESCRIPTION with type state = Tiles.board
                                     and type move = Tiles.direction) = 
    MakeTilePuzzleDescription (struct
                                let initial = solved
                                let dims = cDIMS
                              end) in
  let rec make_moves n b = 
    if n <= 0 then b
    else make_moves (n - 1) (rand_elt (Puzzle.neighbors b)) in
  make_moves cINITIAL_MOVE_COUNT Puzzle.initial_state ;;

module BigTilePuzzle = MakeTilePuzzleDescription (struct
          let initial = random_tileboard () 
          let dims = cDIMS
      end)

module BigTileDFS = DFSSolver(BigTilePuzzle) ;; 
module BigTileBFS = BFSSolver(BigTilePuzzle) ;;
module BigTileBFSFaster = FastBFSSolver(BigTilePuzzle) ;;

print_string "\nTile DFS: " ;;
Absbook.call_reporting_time BigTileDFS.solve () ;;
print_string "Tile List BFS: " ;;
Absbook.call_reporting_time BigTileBFS.solve () ;;
print_endline "Tile Stack BFS: " ;;
Absbook.call_reporting_time BigTileBFSFaster.solve() ;;
print_string "\n"