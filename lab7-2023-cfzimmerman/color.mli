type color = int * int * int

type color_name = 
| Red
| Green
| Blue
| Orange
| Yellow
| Indigo
| Violet

val to_color : int -> int -> int -> color

val red : color -> int

val green : color -> int

val blue : color -> int

val color_named : color_name -> color


