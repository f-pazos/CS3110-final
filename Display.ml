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
  let rec poly_helper regs = match regs with 
    | [] -> ()
    | (_, h)::t -> begin
      print_endline (string_of_int (h.base_color));
      set_color h.base_color;
      print_endline (string_of_int( Array.length h.polygon));
      fill_poly h.polygon;
      set_color black;
      draw_poly h.polygon;
      poly_helper t
    end in
  let rec tribe_helper tribes y = match tribes with
    | [] -> ()
    | (n,tr)::tl -> begin
      set_color black;
      moveto 820 y;
      draw_string n;
      moveto 840 (y-15);
      let s1 = "Pop: "^(string_of_int tr.pop)^" | Food: "^(string_of_int tr.food) in
      draw_string s1;
      moveto 840 (y-30);
      let s2 = "Tools: "^(string_of_int tr.tools)^" | Weapons: "^(string_of_int tr.weps) in
      draw_string s2;
      tribe_helper tl (y-40)
    end in
  set_color black;
  poly_helper st.regions;
  tribe_helper st.tribes 780


  
                
