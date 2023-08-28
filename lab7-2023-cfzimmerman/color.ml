(*
                              CS51 Lab 7
                          Modules & Functors

                 A module for colors and color names

The representation for colors in this implementation of the signature
in `color.mli` is really obscure (and arguably unnecessarily so). By
the way, it also has some bugs so it doesn't pass all the unit
tests. No need to debug it though, or even read it. You'll be
replacing it wholesale with your own hopefully simpler
implementation. *)

type color = int * int * int ;;

type color_name =
  | Red
  | Green
  | Blue
  | Orange
  | Yellow
  | Indigo
  | Violet ;;

(* to_color r g b -- Returns the `color` corresponding to the RGB values
   given by `r`, `g`, and `b` *)

(*

To pass the unit tests, you'll want the RGB values for the colors in
the `color_name` type to have the following values:

  R  |  G  |  B  | Color
----|-----|-----|--------
255 |   0 |   0 | Red
  0 | 255 |   0 | Green
  0 |   0 | 255 | Blue
255 | 165 |   0 | Orange
255 | 255 |   0 | Yellow
  75 |   0 | 130 | Indigo
240 | 130 | 240 | Violet

*)

let to_color (r : int) (g : int) (b : int) : color =
  (r, g, b) ;;

(* red c / green c / blue c -- Returns the corresponding channel value
   for the color `c` *)
let red ((r, _, _) : color) : int = r ;;

let green ((_, g, _) : color) : int = g ;;

let blue ((_, _, b) : color) : int = b;;

(* color_named name -- Returns the color corresponding to the color
   `name` *)
let color_named (name : color_name) : color =
  match name with
  | Red -> (255, 0, 0)
  | Green -> (0, 255, 0)
  | Blue -> (0, 0, 255)
  | Orange -> (255, 165, 0)
  | Yellow -> (255, 255, 0)
  | Indigo -> (75, 0, 130)
  | Violet -> (240, 130, 240) ;;