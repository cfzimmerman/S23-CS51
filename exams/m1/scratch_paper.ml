let rec fac (num : float) : float =
  if num < 0. then raise (Failure "num must be 0 or greater") 
  else if num = 0. then 1. 
  else num *. (fac (num -. 1.))
;;

let combo (n : float) (r : float) : float =
  if n < r then 0. else
  (fac n) /. ((fac r) *. (fac (n -. r)))
;;

(* This joint PMF is specific to ES 150 PS5 Q4 PT.A *)
let joint_pmf (x : float) (y : float) : float =
  (combo 3. x) *. (combo (3. -. x) y) *. (Float.pow (1./.6.) x) *. (Float.pow(1./.2.) y) *. (Float.pow (1./.3.) (3. -. x -. y))
;;

let print_answers (x_min, x_max : int * int) (y_min, y_max : int * int) =
  for x = x_min to x_max do 
    print_string "\n" ;
    for y = y_min to y_max do 
      Printf.printf "x = %d, y = %d: %.*f \n" x y 12 (joint_pmf (float_of_int x) (float_of_int y)) 
    done
  done
;;

let pxy (x : float) (y: float) : float =
  x *. y *. (joint_pmf x y)
;;

let rec ev_xy (x_min, x, x_max : float * float * float) (y_min, y, y_max : float * float * float) : float =
    if y <= y_max then 
      let p_xy = pxy x y in
      begin 
        Printf.printf "x = %.*f, y = %.*f: %.*f \n" 0 x 0 y 12 p_xy ;
        p_xy +. ev_xy (x_min, x, x_max) (y_min, y +. 1., y_max) 
      end
    else if x < x_max 
      then ev_xy (x_min, x +. 1., x_max) (y_min, y_min, y_max)
    else 0.
;;
 
print_answers (0, 3) (0, 3) ;;

(*

OUTPUT: PS5 Q4(A)

# print_answers (0, 3) (0, 3) ;;

x = 0, y = 0: 0.037037037037 
x = 0, y = 1: 0.166666666667 
x = 0, y = 2: 0.250000000000 
x = 0, y = 3: 0.125000000000 

x = 1, y = 0: 0.055555555556 
x = 1, y = 1: 0.166666666667 
x = 1, y = 2: 0.125000000000 
x = 1, y = 3: 0.000000000000 

x = 2, y = 0: 0.027777777778 
x = 2, y = 1: 0.041666666667 
x = 2, y = 2: 0.000000000000 
x = 2, y = 3: 0.000000000000 

x = 3, y = 0: 0.004629629630 
x = 3, y = 1: 0.000000000000 
x = 3, y = 2: 0.000000000000 
x = 3, y = 3: 0.000000000000 
- : unit = ()

*)

ev_xy (0., 0., 3.) (0., 0., 3.) ;;

(*

OUTPUT: PS5 Q4(D)

# ev_xy (0., 0., 3.) (0., 0., 3.) ;;

x = 0, y = 0: 0.000000000000 
x = 0, y = 1: 0.000000000000 
x = 0, y = 2: 0.000000000000 
x = 0, y = 3: 0.000000000000 
x = 1, y = 0: 0.000000000000 
x = 1, y = 1: 0.166666666667 
x = 1, y = 2: 0.250000000000 
x = 1, y = 3: 0.000000000000 
x = 2, y = 0: 0.000000000000 
x = 2, y = 1: 0.083333333333 
x = 2, y = 2: 0.000000000000 
x = 2, y = 3: 0.000000000000 
x = 3, y = 0: 0.000000000000 
x = 3, y = 1: 0.000000000000 
x = 3, y = 2: 0.000000000000 
x = 3, y = 3: 0.000000000000 
- : float = 0.5

*)

