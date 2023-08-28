open Graphics

module type FILTER_TYPE = sig
  type image_type

  (* threshold_filter -- image where pixels above the threshold value are black *)
  val threshold_filter : image_type -> image_type

  (* dithered image *)
  val dither : image_type -> image_type

  (* show the image *)
  val depict : image_type -> unit
end

module Filter = struct
  (* images are lists of lists of floats between 0. (white) and 1. (black) *)
  type image_type = float list list

  let threshold_filter img threshold =
    List.map
      (fun row -> List.map (fun v -> if v <= threshold then 0. else 1.) row)
      img

  let dither img =
    List.map
      (fun row ->
        List.map (fun v -> if v > Random.float 1. then 1. else 0.) row)
      img

  let get_window_dims (img : image_type) : int * int =
    (List.length (List.hd img), List.length img)

  let depict_pix (lvl_percent : float) (y_current : int) (x_current : int)
      (y_max : int) : unit =
    let lvl = int_of_float (255. *. (1. -. lvl_percent)) in
    Graphics.set_color (Graphics.rgb lvl lvl lvl);
    Graphics.plot x_current (y_max - y_current)

  let depict (img : image_type) : unit =
    Graphics.open_graph "";
    Graphics.clear_graph ();
    let x_max, y_max = get_window_dims img in
    Graphics.resize_window x_max y_max;

    List.iteri
      (fun r rw -> List.iteri (fun c pix -> depict_pix pix r c y_max) rw)
      img;
    Unix.sleep 2;
    Graphics.close_graph ()
end
