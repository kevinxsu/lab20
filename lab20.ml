open Graphics ;;

(* images are lists of lists of floats between 0. (white) and 1. (black) *)
type image = float list list ;;
type size = int * int ;;
  
(* threshold thershold image -- image where pixels above the threshold
value are black *)
let threshold img threshold =
  List.map  (fun row -> List.map (fun v -> if v <= threshold then 1. else 0. ) row) img
       
(* show the image *)
let depict (img : image) : unit =
  Graphics.open_graph ""; 
  Graphics.clear_graph ();
  let x, y = List.length (List.hd img), List.length img in
  Graphics.resize_window x y;
  let depict_pix v r c = 
    let lvl = int_of_float (255. *. (1. -. v)) in 
    Graphics.set_color (Graphics.rgb lvl lvl lvl);
    plot c (y - r) 
  in
  List.iteri (fun r row -> List.iteri (fun c pix -> depict_pix pix r c) row) img;
  Unix.sleep 2; 
  Graphics.close_graph () ;;

(* dither max image -- dithered image *)
let dither img =
  List.map (fun row -> List.map (fun v -> if v > Random.float 1. then 1. else 0.) row) img ;;
    
let mona = Monalisa.image ;;
let mona_threshold = threshold mona 0.75 ;;
let mona_dither = dither mona ;;

depict mona ;;
depict mona_threshold ;;
depict mona_dither ;;