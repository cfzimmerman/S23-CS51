(*
                          CS 51 Problem Set
                 Simulation of an Infectious Disease

                      People in the simulation
 *)

module G = Graphics
open Config
open Registry
module Ctr = Counter
module Viz = Visualization
module Stat = Statistics

(* module Utilities = Utilities ;; *)

(*....................................................................
                                People
*)

(* Returns a positive integer sampled from a Gaussian Random Variable. *)

let gaussian_counter (mean : float) (stdev : float) : Ctr.counter =
  let pos_gaussian_rand (mean : float) (stdev : float) : int =
    Int.abs (int_of_float (Utilities.gaussian mean stdev))
  in

  let new_counter = new Ctr.counter in
  new_counter#set (pos_gaussian_rand mean stdev);
  new_counter

let colored_point ((x, y) : int * int) (color : int) = Viz.draw_circle x y color

class person (initx : int) (inity : int) (initstepsize : int)
  (initinfect : float) =
  object (self)
    val id : string = Utilities.gensym ()

    val mutable posx : int = initx

    val mutable posy : int = inity

    val mutable step_size : int = initstepsize

    val mutable infectiousness : float = initinfect

    method id : string = id

    method step_size : int = step_size

    method infectiousness : float = infectiousness

    method set_pos (x : int) (y : int) : unit =
      posx <- x;
      posy <- y

    method pos = (posx, posy)

    method set_step_size (new_step_size : int) : unit =
      step_size <- new_step_size

    method set_infectiousness (new_infect : float) : unit =
      infectiousness <- new_infect

    method move : unit =
      let x, y = self#pos in
      let newx, newy = Utilities.rand_step x y self#step_size in
      (* drop from old location in registry *)
      Reg.deregister (self :> thing_type);
      (* update location *)
      self#set_pos newx newy;
      (* re-add at the new location *)
      Reg.register (self :> thing_type)

    method update : unit = self#move

    method draw : unit = colored_point self#pos G.black
  end

(*....................................................................
                       People in various states

  Note that since these classes refer to each other, they must be
  simultaneously defined using `and` instead of sequentially defined
  as separate classes.
*)

class susceptible (initx : int) (inity : int) =
  object (self)
    inherit
      person initx inity cSTEP_SIZE_SUSCEPTIBLE cINFECTIOUSNESS_SUSCEPTIBLE as super

    initializer Stat.susceptible#bump

    method! update =
      super#update;
      let posx, posy = self#pos in
      (* calculate total infectiousness of all neighbors *)
      let infectiousness_total =
        Utilities.sum_float
          (List.map
             (fun obj -> obj#infectiousness)
             (Reg.neighbors (self :> thing_type)))
      in
      (* if infected, update the registry by replacing this object
         with an infected one *)
      if Utilities.flip_coin infectiousness_total then (
        Stat.susceptible#debump;
        Reg.deregister (self :> thing_type);
        Reg.register (new infected posx posy :> thing_type))

    method! draw = colored_point self#pos cCOLOR_SUSCEPTIBLE
  end

and (* class *) infected (initx : int) (inity : int) =
  object (self)
    inherit
      person initx inity cSTEP_SIZE_INFECTED cINFECTIOUSNESS_INFECTED as super

    initializer Stat.infected#bump

    val recovery_counter : Ctr.counter =
      gaussian_counter (fst cRECOVERY_PERIOD) (snd cRECOVERY_PERIOD)

    method! update =
      (* My extension: Mortality can occur at any time during the infection. *)
      super#update;
      if recovery_counter#count > 0 && not (Utilities.flip_coin cMORTALITY) then 
        (* Normal step towards recovery *)
        recovery_counter#debump
      else 
        begin
          begin 
            Stat.infected#debump;
            Reg.deregister (self :> thing_type);
          end;
          if recovery_counter#count = 0 then 
            Reg.register (new recovered posx posy :> thing_type)
          else 
            Reg.register (new deceased posx posy :> thing_type)
        end


    method! draw =
      let x, y = self#pos in
      colored_point (x, y) cCOLOR_INFECTED;
      Viz.draw_circle
        ~size:(cSYMBOL_SIZE + cNEIGHBOR_RADIUS)
        ~filled:false x y cCOLOR_INFECTED
  end

and (* class *) recovered (initx : int) (inity : int) =
  object (self)
    inherit
      person initx inity cSTEP_SIZE_RECOVERED cINFECTIOUSNESS_RECOVERED as super

    initializer Stat.recovered#bump

    val immunity_counter : Ctr.counter =
      gaussian_counter (fst cIMMUNITY_PERIOD) (snd cIMMUNITY_PERIOD)

    method! update =
      super#update;
      if immunity_counter#count = 0 then (
        Stat.recovered#debump;
        Reg.deregister (self :> thing_type);
        Reg.register (new susceptible posx posy :> thing_type))
      else immunity_counter#debump

    method! draw = colored_point self#pos cCOLOR_RECOVERED
  end

and (* class *) deceased (initx : int) (inity : int) =
  object (self)
    inherit
      person initx inity cSTEP_SIZE_DECEASED cINFECTIOUSNESS_DECEASED

    initializer Stat.deceased#bump

    method! draw =
      let x, y = self#pos in 
       Viz.draw_cross x y cCOLOR_DECEASED
  end
