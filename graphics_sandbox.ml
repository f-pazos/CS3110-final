(* File to play around with how graphics work in OCaml, free of the rest 
 * of the project. *)

open Graphics
open Display

let random_poly () = 
  let n = Random.int 10 in 

  let arr = Array.make n (0,0) in 

  let rec init_arr a i = 
    if (i = -1) then a else
    (Array.set a i (Random.int 500, Random.int 600);
    init_arr a (i-1)) in

  init_arr arr (n-1)

let random_color () = 
  let colors = [black; white; red; green; blue; yellow; cyan; magenta] in 
  List.nth colors (Random.int 7)

let random_region () : map_region = 
  {
    polygon = random_poly ();
    color = random_color ();
    name = "xyzzy";
    area = 0.0;

    neighbors = [];
    edge_lengths = [];
  }

let rec main () = 
  let w = [
    random_region ();
    random_region ();
    random_region ();
    random_region ();
    random_region () 
  ] in 

  display {regions = w};
  ignore (read_line() );
  main ()

let () = 
  main ();



