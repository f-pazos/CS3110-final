open Graphics
open State

let win_w = 1000
let win_h = 800

(* TODO: Implement alter_color *)
(* [alter_color col st] Alters color [col] based on the state
 * [st] to provide some information of the state graphically. *)
let alter_color col st = ()

let display_init () = 
  open_graph ""; 
  resize_window win_w win_h;
  ()


(* [display st] provides a graphic representation for [st]. *)
let display st = 
  let rec display_helper regs text_y = match regs with 
    | [] -> ()
    | (_, h)::t -> begin
                print_endline (string_of_int (h.base_color));
                set_color h.base_color;
                print_endline (string_of_int( Array.length h.polygon));
                fill_poly h.polygon;
                set_color black;
                draw_poly h.polygon;
                moveto 820 text_y;
                draw_string h.name;
                display_helper t (text_y - 30)
              end in 

  set_color black;
  display_helper st.regions 770


  
                
