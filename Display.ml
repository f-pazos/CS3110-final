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

(* [com p] Returns the center of mass of polygon p as an (int*int) *)
let com p: (int*int)= 
  let n = Array.length p in 

  (* Find the sum of x and y values of outline o. *)
  let x_sum = ref 0 in 
  let y_sum = ref 0 in 

  for i = 0 to n-1 do 
    x_sum := (!x_sum) + fst (p.(i));
    y_sum := (!y_sum) + snd (p.(i))
  done;

  ( (!x_sum) / n, (!y_sum) / n)


(* [display st] provides a graphic representation for [st]. *)
let display (st:state) = 
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

  let rec draw_overlay regs = match regs with 
    | [] -> ()
    | (n, r)::t -> begin
        let (xcom, ycom) = com r.polygon in 

        (* Figure out how long the name is *)
        moveto xcom ycom;
        draw_string r.name;
        let (currX, currY) = current_point () in 

        set_color r.base_color; 
        fill_rect xcom ycom (currX - xcom + 10) 20;
        set_color black;
        draw_rect xcom ycom (currX - xcom + 10) 20;
        moveto (xcom+7) (ycom+5);
        draw_string r.name;

        draw_overlay t

      end in 

  set_color black;
  poly_helper st.regions;
  tribe_helper st.tribes 780;
  draw_overlay st.regions


  
                
