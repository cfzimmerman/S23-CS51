open Filter ;;
module Fl = Filter ;;
module Mo = Monalisa ;;

let depict_monas () : unit =
  let mona : Fl.image_type = Mo.image in
  let mona_threshold : Fl.image_type = Fl.threshold_filter mona 0.75 in
  let mona_dither : Fl.image_type = Fl.dither mona in
  Fl.depict mona;
  Fl.depict mona_threshold;
  Fl.depict mona_dither
;;

depict_monas ()
