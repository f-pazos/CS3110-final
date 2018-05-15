(* Point of entry to dummy program that generates and displays a state *)
open Display
open Generator 

let gui_main () = 
  let st0 = generate_state 20 1 1 in 
  display_init ();
  display st0;
  ignore (read_line () )



let () = gui_main ()
