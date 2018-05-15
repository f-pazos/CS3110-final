open Graphics
open State

let debug_display = false 

let win_w = 1000
let win_h = 800

(* TODO: Implement alter_color *)
(* [alter_color col st] Alters color [col] based on the state
 * [st] to provide some information of the state graphically. *)
let alter_color col st = ()

(* [text_color c] Returns the best color to use for text given a 
 * background color [c]. *)
let text_color c = 
  let blue = c mod 256 in 
  let green = (c/256) mod 256 in 
  let red = (c/(256*256)) mod 256 in 

  (* If the color is closer to white, return black and vice versa *)
  if (blue + green + red)/3 > 100 then 0x000000 else 0xFFFFFF 


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

      if debug_display 
      then 
        (print_endline (string_of_int (h.base_color));
        print_endline (string_of_int( Array.length h.polygon)))
      else ();


      set_color h.base_color;
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
        let (textw, texth) = text_size r.name in 

        (* (x,y) is the bottom left corner of the text box. *)
        let (x,y) = (xcom - textw/2, ycom-texth/2) in 

        (* Draw the text box and name. *)
        set_color r.base_color; 
        fill_rect (x-6) (y-5) (textw + 10) (texth + 10);
        set_color black;
        draw_rect (x-6) (y-5) (textw + 10) (texth + 10);
        moveto x y;

        set_color (text_color (r.base_color));
        draw_string r.name;

        (* Draw info overlay *)

        let (xover, yover) = (xcom - 15, ycom - 50) in 
        set_color r.base_color;
        fill_rect xover yover 30 30;
        set_color black;
        draw_rect xover yover 30 30;

        draw_overlay t

      end in 

  set_color black;
  poly_helper st.regions;
  tribe_helper st.tribes 780;
  draw_overlay st.regions


  
                
