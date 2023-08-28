
(* 

Exercise 0: 

(a) We want to write a class type for a “counter” object that maintains an integer 

state that can be “bumped” by adding an integer.

(b) Write a class definition satisfying this interface the counter_interface above 

*)

class type ctr =
  object 
    method bump : int -> unit 
    method get_ct : int
  end

class counter : ctr =
  object (this)
    val mutable ct : int = 0 
    method bump (num : int) : unit = ct <- ct + num
    method get_ct: int = ct
  end


(* 

Exercise 1


Write a class definition for a class loud_counter obeying the same interface that works
identically, except that it also prints the resulting state of the counter each time the
counter is bumped. 

*)

class loud_counter  =
  object
    inherit counter as super
    method! bump (num : int) : unit = 
      super#bump num ;
      Printf.printf "%i\n" super#get_ct
  end 


(* 

Exercise 2

Write a class type definition for an interface reset_counter_interface, which is
just like counter_interface except that it has an additional method of no arguments
intended to reset the state back to zero. 

*)

class type reset_counter_interface =
  object inherit ctr 
    method reset : unit
  end


(* 

Exercise 3


Write a class definition for a class loud_reset_counter satisfying the reset_counter_-
interface that implements a counter that both allows for resetting and is “loud”
(printing the state whenever a bump or reset occurs). 

*)

class loud_reset_counter : reset_counter_interface = 
  object(this)
    inherit loud_counter as super
    method reset : unit =
      super#bump (super#get_ct * -1)
  end


(*

let c = new loud_reset_counter
increment (c :> counter_interface)

*)

(* Subclasses and Subtypes  *)


(*  

The goal of this exercise is to make classes that represent uses on a social media 

site. 

Exercise 1: Create a class type for a user that supports the follwoing methods


1) Getting a user's username

2) Getting a user's id 

3) Adding a friend to a user's list of friends

4) Retrive Friends

5) Add a post (a string) to a user's list of posts 

6) Remove a post 

7) Retrieve all the user's post

*)


class type user_interface =
  object
    method get_username : string
    method get_user_id : string 
    method add_friend : string -> unit
    method list_friend_ids : unit -> string list 
    method create_post : string -> unit
    method remove_post : string -> unit
    method list_posts : unit -> string list
  end

(*  

Exercise 2: Define the implementation for a class that satisfies the user signature

The class should take in a user's username and id

*)

class user (username : string) (user_id: string) : user_interface =
  object (this)
    val mutable friend_list = []
    val mutable post_list = []

    method get_username : string = username
    method get_user_id : string = user_id 
    method add_friend (friend_id : string) : unit = 
      friend_list <- (friend_id :: friend_list)
    method list_friend_ids () : string list =
      friend_list
    method create_post (post_id : string) : unit =
      post_list <- (post_id :: post_list)
    method remove_post (post_id : string) : unit =
      post_list <- List.filter ((<>) post_id) post_list
    method list_posts () : string list =
      post_list
  end



(*  

Exercise 3: 



(a) 


(a) Define a class type called student that has the same functionality 

as the user class but supports an additional 

method whcih output a school a student attends

Define the implementation for a class called student which contains 

all the same functionality as the user class with the additional method: 

1) A method to return the school that the student attends (this class should take 

an additional argument with the school a student goes to) 


*)

class type student_interface =
  object inherit user 
    method get_school : string
  end

class student (username : string) (user_id: string) (school : string) =
  object (this)
    inherit user username user_id as super 
    method get_school : string = school
end

(* 


Exercise 4: 

(a) Define a function form_friend_group that takes in a list of users, and for each 

user in the list, sets their followers to be everyone else in the list


(b) Define a list of four students and call form_friend_group on that list

*)

let rec add_friends_helper (all_users : user list) (current_user : user) : user =
  match all_users with 
  | [] -> current_user
  | hd :: tl -> 
    if hd#get_user_id <> current_user#get_user_id then 
      current_user#add_friend (hd#get_user_id);
    add_friends_helper tl current_user
;; 

let add_friends (all_users : user list) (current_user : user) : unit =
  let _ = add_friends_helper all_users current_user in ()
;;

(* Can this be done with List.fold left? *)
let form_friend_group (users : user list) : unit =
  List.iter (add_friends users) users
;;

let friends: student list = [
  new student "Athos" "1" "School";
  new student "Porthos" "2" "School";
  new student "Aramis" "3" "School";
  new student "Planchet" "4" "School"
] ;;

form_friend_group (friends :> user list )
