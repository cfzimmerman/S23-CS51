(* Q1.1 *)
let y = 6 in 
  let f x = x * x 
in f y + y ;;

let x = 6 in 
  let x = 7 in 
    x * 6 ;;

List.fold_right (/) [5; 10; 2] 4200 ;;

List.fold_left (/) 4200 [5; 10; 2];

(* Return to 🚨 2.6  *)
let y = 7 in 
  let rec f x = 
    if x > y then f (x / 2)
    else if x < y then f (succ x) 
    else x * x in 
  f 100 ;;

(* Return to 🚨 2.7  *)

(* 4.1 *)
let raises_exception (f : unit -> 'a) : bool = 
  try 
    let _ = f() in false
  with
    | _ -> true

let failure () =
  raise (Failure "Nope")
;;

let raises_exception_applied = raises_exception failure ;;

raises_exception (fun () -> raise (Failure "Nope")) ;;

raises_exception (fun () -> "success!") ;;

(* Q5 

 if left < 1 || above < 1 then raise (Invalid_argument "Indicies must be greater than one")

*)

let rec pascal (left : int) (above : int) : int =
  if left < 0 || above < 0 then raise (Invalid_argument "Out of bounds") 
  else if left = 0 || above = 0 then 1
  else if (left = 1) then above + 1
  else if (above = 1) then left + 1
  else (pascal (left - 1) above) + (pascal left (above - 1))
;;


(* Q6 *)

type ('left, 'right) combo =
  | Neither 
  | Left of 'left 
  | Right of 'right 
  | Both of 'left * 'right ;;

let combo_example = Both(true, 3.14)

let rec left_list (lst : 'a list) : ('a, 'b) combo list =
  match lst with 
  | [] -> []
  | hd :: tl -> Left(hd) :: (left_list tl)
;;

let rec right_list (lst : 'a list) : ('a, 'b) combo list =
  match lst with 
  | [] -> []
  | hd :: tl -> Right(hd) :: (left_list tl)
;;

let rec zip_combo (lefts : 'left list) (rights : 'right list) : ('left, 'right) combo list =
  match lefts, rights with 
  | [], [] -> []
  | [], hd :: tl -> Right(hd) :: (zip_combo [] tl)
  | hd :: tl, [] -> Left(hd) :: (zip_combo tl [])
  | h1 :: t1, h2 :: t2 -> Both(h1, h2) :: (zip_combo t1 t2)
;;

(* Q7 *)

module type DIRECTORY =
  sig
    (* The type of file contents *)
    type contents
    (* The type of nodes, both directories and files *)
    type node

    (* new_dir str contents -- Returns a new directory node named
       `str`. *)
    val new_dir : string -> node
    (* new_file str contents -- Returns a new file node named `str`
       holding the given `contents`. *)
    val new_file : string -> contents -> node
    (* is_dir node -- Returns `true` just in case `node` is a
       directory node. *)
    val is_dir : node -> bool
    (* add_node new_node dir -- Returns a directory node that augments
       directory node `dir` with an additional node `new_node`. Raises 
       an `Invalid_argument` exception if `dir` is not a directory 
       node. *)
    val add_node : node -> node -> node
    (* node_at start path -- Returns the node within `start` at the
       given `path`, which is a list of node names, as an option
       value.  Raises `Not_found` if no such node exists. *)
    val node_at : node -> string list -> node
    (* contents_of node -- Returns a string representation of the
       contents of the `node` if a file node; or if a directory,
       returns the name of the directory. *)
    val contents_of : node -> string
    (* finds_path target_name node -- Returns a path, a list of names
       of nodes, that constitute the path to a node named `target`
       within the given `node`. The return value is `None` if no such
       node exists within `node`, and `Some ...path...` otherwise.*)
    val find_path : string -> node -> string list option
  end ;;

  module type FILE_CONTENTS =
  sig
    type t
    val string_of : t -> string
  end ;;
  (*
      module MakeStack (Element: SERIALIZE)
  : (STACK with type element = Element.t) = 
  *)

  module MakeDirectory (Contents : FILE_CONTENTS) : (DIRECTORY with type contents = Contents.t) =
  struct
    
    type contents = Contents.t
      
    type node =
      | Directory of string * node list
      | File of string * contents
			   
    let is_dir (node : node) : bool =
      match node with
      | File _ -> false
      | Directory _ -> true
			 
    let new_dir (name : string) : node =
      Directory (name, [])
		
    let new_file (name : string) (contents : contents) : node =
      File (name, contents)
	   
    let name_of (node : node) : string =
      match node with
      | File (name, _)
      | Directory (name, _) -> name
					
    let add_node (node : node) (in_dir : node) : node =
      match in_dir with
      | File _ -> raise (Invalid_argument "can't add file to a non-directory")
      | Directory (dir_name, nodes) ->
	 Directory (dir_name, node :: nodes)
		   
    let node_at (root : node) (path : string list) : node =
      
      let node_named target nodes =
	List.find (fun node -> name_of node = target) nodes in
      
      let rec node_at' roots path =
	match path with
	| [] -> raise (Failure "node_at: empty path")
	| name :: tl ->
	   let next = node_named name roots in
	   if tl = [] then next
	   else
	     (match next with
	      | Directory (name, subnodes) -> node_at' subnodes tl
	      | File _ -> raise (Failure "node_at: no such path")) in

      node_at' [root] path
		    
    let contents_of (node : node) : string =
      match node with
      | File (_name, contents) -> Contents.string_of contents
      | Directory (name, _subnodes) -> name
					 
    let first_some lst =
      List.nth_opt (List.concat (List.map Option.to_list lst)) 0
		   
    let rec find_path (target : string) (node : node) : string list option =
      match node with
      | File (name, contents) ->
	 if target = name then Some [name]
	 else None
      | Directory (name, nodes) ->
	 if target = name then Some [name]
	 else
	   let subpath = first_some (List.map (find_path target) nodes) in
	   match subpath with
	   | None -> None
	   | Some subpath_val -> Some (name :: subpath_val)
  end ;;


(*
   
module IntSerialize : (SERIALIZE with type t = int) =
  struct
    type t = int
    let serialize = string_of_int
  end ;;

module IntStack : (STACK with type element = IntSerialize.t) =
  MakeStack (IntSerialize) ;;


*)

module IorSDir : (FILE_CONTENTS with type t = (int, string) combo) =
  struct
    type t = (int, string) combo
    let string_of (el : t) : string = 
      match el with 
      | Left(num) -> string_of_int(num)
      | Right(text) -> text
      | Both(num, text) -> "(" ^ string_of_int(num) ^ ", " ^ text ^ ")"
      | Neither -> ""
  end ;;

module IntOrStringDirectory : (DIRECTORY with type contents = IorSDir.t) =
  MakeDirectory(IorSDir) ;;

let example : IntOrStringDirectory.node = 
  let open IntOrStringDirectory in 
    add_node (add_node (IntOrStringDirectory.new_dir "root") (new_file "file1" Neither)) (new_file "file2" Neither)
;;
