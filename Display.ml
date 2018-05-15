open Graphics
open State

let debug_display = false 

(* Window dimensions. Map has dimensions 800*800, tribe list has dim. 300*800, 
 * and legend has dim 300*800. *)
let win_w = 1400
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


let draw_icon_circle x y w =  
  set_color white;
  fill_circle (x+w/2) (y+w/2) (w/2);
  set_color black;
  draw_circle  (x+w/2) (y+w/2) (w/2)

(* Draws a food icon with lower left corner at (x,y), with width of w. *)
let draw_food_icon x y w =

  draw_icon_circle x y w; 

  set_color yellow;
  fill_ellipse (x+w/2) (y+w/2) (w/7) (w*3/8);
  set_color black;
  draw_ellipse (x+w/2) (y+w/2) (w/7) (w*3/8);

  set_color green;

  let leaves = [| ( (x+w/2),(y+w/4+w/8) );
                ( (x+w/2 + w/4), (y+w/2+w/8) );
                ( (x+w/2),(y+ w/12) );
                ( (x+w/2 - w/4), (y+w/2+w/8) ) |] in 

  fill_poly leaves;
  set_color black;
  draw_poly leaves

(* Draws a tool icon with lower left corner at (x,y) with width of w. *)
let draw_tool_icon x y w = 
  draw_icon_circle x y w; 

  (* centers *)
  let xc = x+w/2 in 

  let handle = [| ( (xc-w/16), (y+w/6) );
                  ( (xc-w/16), (y+5*w/6) );
                  ( (xc+w/16), (y+5*w/6) );
                  ( (xc+w/16), (y+w/6) ) |] in 

  let stone = [| ( (x+w/5), (y+13*w/16) );
                 ( (x+w/5), (y+5*w/8) );
                 ( (x+4*w/5), (y+5*w/8) );
                 ( (x+4*w/5), (y+13*w/16) ) |] in 
  (* brown *)
  set_color 0x542d05;
  fill_poly handle;
  set_color black;
  draw_poly handle;

  (* gray *)
  set_color 0xa5a4a4;
  fill_poly stone;
  set_color black;
  draw_poly stone

(* Draws a weapon icon with lower left corner at (x,y) with width of w. *)
let draw_weapon_icon x y w = 
  draw_icon_circle x y w;

  let xc = x+w/2 in 
  let yc = y+w/2 in 

  let x_handle_l = xc-w/16 in 
  let x_handle_r = xc+w/16 in 
  let x_leftmost = x+w/3 in 
  let x_rightmost = x+2*w/3 in 

  let y_handle_top = yc-w/6 in 
  let y_handle_mid = yc-w/6-w/16 in 
  let y_handle_bot = y+w/16 in 

  let handle = [| (x_handle_l, y_handle_bot); 
                  (x_handle_l, y_handle_mid);
                  (x_leftmost, y_handle_mid);
                  (x_leftmost, y_handle_top);
                  (x_rightmost, y_handle_top);
                  (x_rightmost, y_handle_mid);
                  (x_handle_r, y_handle_mid);
                  (x_handle_r, y_handle_bot) |] in 
  
  let blade = [| (x_handle_l, y_handle_top); 
                 (xc-w/8, yc+w/4);
                 (xc, y+w-w/16);
                 (xc+w/8, yc+w/4);
                 (x_handle_r, y_handle_top) |] in 

  (* brown *)
  set_color 0x542d05; 
  fill_poly handle; 
  set_color black;
  draw_poly handle;

  (* gray *)
  set_color 0xa5a4a4;
  fill_poly blade;
  set_color black;
  draw_poly blade

(* Draws an attack icon with lower left corner at (x,y) with width of w. *)
let draw_attack_icon x y w = 
  draw_icon_circle x y w; 

  let xc = x+w/2 in 
  let yc = y+w/2 in

  set_color black; 
  
  let  handle = [| 
              (x+6*w/16), (y+5*w/16);
              (x+4*w/16, y+3*w/16);
              (x+3*w/16, y+4*w/16);
              (x+5*w/16, y+6*w/16);
              (x+9*w/16, y+4*w/16);
              (x+8*w/16, y+3*w/16);
              (x+3*w/16, y+8*w/16);
              (x+4*w/16, y+9*w/16)
               |] in

  let blade = [| (x+5*w/16, y+6*w/16);
                 (x+6*w/16, y+5*w/16); 
                 (xc+w/8+w/16, yc+w/8-w/16);
                 (xc+w/4, yc+w/4);
                 (xc+w/8-w/16, yc+w/8+w/16)
                  |] in 


  (* attack lines *)
  moveto (x+w/4) (y+3*w/4);
  lineto (xc-w/8) (yc+w/8);

  moveto (x+w/4+w/16) (y+3*w/4+w/16);
  lineto (xc-w/8+w/16) (yc+w/8);

  moveto (x+w/4+2*w/16) (y+3*w/4+2*w/16);
  lineto (xc-w/8+w/7) (yc+w/8+w/32);

  (* gray *)
  set_color 0xa5a4a4;
  fill_poly blade;
  set_color black;
  draw_poly blade;

  (* brown *)
  set_color 0x542d05; 
  fill_poly handle;
  set_color black;
  draw_poly handle


(* Draws a gift icon with lower left corner at (x,y) with width of w. *)
let draw_gift_icon x y w = 
  draw_icon_circle x y w;

  let xc = x+w/2 in 
  let yc = y+w/2 in 

  (* Bottom of gift box *)
  set_color red;
  fill_rect (x+w/5) (y+w/5) (3*w/5) (3*w/8);
  set_color black;
  draw_rect (x+w/5) (y+w/5) (3*w/5) (3*w/8);

  (* top of gift box *)
  set_color red;
  fill_rect (x+w/7) (yc + 3*w/40) (5*w/7) (7*w/40);
  set_color black;
  draw_rect (x+w/7) (yc + 3*w/40) (5*w/7) (7*w/40);

  (* green stripe *)
  set_color green;
  fill_rect (xc-w/12) (y+w/5) (w/6) (3*w/4 - w/5);
  set_color black;
  draw_rect (xc-w/12) (y+w/5) (w/6) (3*w/4 - w/5);

  (* green bow *)
  let bow = [| (xc, y + 3*w/4); 
               (xc-w/4, y + 3*w/4+w/16); 
               (xc-w/6, y + 3*w/4+w/6);
               (xc, y + 3*w/4); 
               (xc+w/16, y+15*w/16);
               (xc+3*w/16, y+13*w/16)  
            |] in 

  set_color green;
  fill_poly bow;
  set_color black;
  draw_poly bow

(* Draws an arros from [r1] to [r2] in color [c]. Uses [st] to find 
 * coordinates, etc. *)
let draw_arrow c r1 r2 st = 

  set_color c; 

  let (x1, y1) = com (List.assoc r1 st.regions).polygon in
  let (x2, y2) = com (List.assoc r2 st.regions).polygon in 

  set_line_width 10;
  moveto x1 y1;
  lineto x2 y2;
  set_line_width 1;
  fill_circle x2 y2 40;
  ()
  
  

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

  let rec tribe_helper tribes regs y = match tribes with
    | [] -> ()
    | (n,tr)::tl -> begin
      let r = (List.assoc n regs) in
      moveto (win_h + 20) y;
      draw_string n;
      let (currX, currY) = current_point () in 
      set_color r.base_color; 
      fill_rect (win_h + 15) (y-2) (currX - (win_h + 10)) 14;
      set_color black;
      draw_rect (win_h + 15) (y-2) (currX - (win_h + 10)) 14;
      moveto (win_h + 20) y;
      draw_string n;
      moveto (win_h + 40) (y-20);
      let s = "Pop:"^(string_of_int tr.pop)^" | Food:"^(string_of_int tr.food)^" | Tools:"^(string_of_int tr.tools)^" | Weapons:"^(string_of_int tr.weps) in
      draw_string s;
      tribe_helper tl regs (y-40)
    end in

  let rec draw_names regs = match regs with 
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
        set_color (text_color (r.base_color));
        draw_rect (x-6) (y-5) (textw + 10) (texth + 10);
        moveto x y;

        set_color (text_color (r.base_color));
        draw_string r.name;
        draw_names t
      end in 

  let rec draw_icons regs st = match regs with 
    | [] -> ()
    | (n ,r)::t -> begin
        let (xcom, ycom) = com r.polygon in 
        let action = (List.assoc n st.tribes).last_action in 
        let w = 30 in 

        (* Bottom left corner of icon *)
        let (x,y) = (xcom-w/2, ycom-w/2-w) in 
        (match action with 
          | Food -> draw_food_icon x y w;
          | Tools -> draw_tool_icon x y w;
          | Weapons -> draw_weapon_icon x y w;
          | Attack _ -> draw_attack_icon x y w;
          | Gift _ -> draw_attack_icon x y w);
        

        draw_icons t st

      end in


  (* Draws arrows to and from regions for gifts and attacks *)
  let rec draw_arrows regs st = match regs with 
    | [] -> () 
    | (n, r)::t -> begin
        let action = (List.assoc n st.tribes).last_action in 
        (match action with 
         | Attack other -> draw_arrow red n other st
         | Gift (other,_) -> draw_arrow green n other st
         | _ -> () );
        draw_arrows t st
      end in 


  let draw_legend = 
    set_color black; 

    (* Draw a dividing line *)
    moveto 1100 800; 
    lineto 1100 0;

    let x_off = 1100 in 

    let (legW, legH) = text_size "Legend" in 

    moveto (x_off + ((300-legW)/2)) 750;
    draw_string "Legend";

    draw_food_icon (x_off + 50) 650 50;
    moveto (x_off + 110) 670; 
    draw_string "Food production";

    draw_tool_icon (x_off + 50) 550 50;
    moveto (x_off + 110) 570; 
    draw_string "Tool production";

    draw_weapon_icon (x_off + 50) 450 50;
    moveto (x_off + 110) 470; 
    draw_string "Weapon production";

    draw_attack_icon (x_off + 50) 350 50;
    moveto (x_off + 110) 370; 
    draw_string "Attack";


    draw_gift_icon (x_off + 50) 250 50 ;
    moveto (x_off + 110) 270; 
    draw_string "Gift" in 

  set_color black;
  poly_helper st.regions;

  (* Refresh tribe info area *)
  set_color white;
  fill_rect 800 0 300 800;
  set_color black;
  draw_rect 800 0 300 800;
  tribe_helper st.tribes st.regions (win_h-10);
  draw_arrows st.regions st;
  draw_icons st.regions st;
  draw_names st.regions;
  draw_legend


  
                
