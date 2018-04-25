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


(* [random_points w h n] creates a list of n random points (x,y) s.t. 
 * 0 <= x < w and 0 <= y < h. *)
let random_points (w:int) (h:int) n = 

  (* Recursive helper function. *)
  let rec rp_help w h n acc = 
    if n = 0 then acc 
    else rp_help w h (n-1) 
        ((float_of_int (Random.int w), float_of_int (Random.int h) )::acc) in 

  rp_help w h n []




(* [order_closest a b points] returns the (float*float) list [points] such that
 * the points are ordered based on the sum of the distances to [pointA] and
 * [pointB]. Essentially - ordering by how close points are to [A] and [B]. *)
let order_closest (x_a, y_a) (x_b, y_b) points = 
  
  points |> 
 
  (* First create an assoc list from total distance to the point. *)
  List.map (fun (x,y) ->
      let tot_dist = sqrt ( (x-.x_a)**2.0 +. (y-.y_a)**2.0) +.
                     sqrt ( (x-.x_b)**2.0 +. (y-.y_b)**2.0) in 
      (tot_dist, (x,y) )) |> 

  (* Sort the list in order of its distance to the point. *)
  List.sort_uniq (fun (d1, _) (d2, _) -> if d1 = d2 then 0 else
                                    if d1 > d2 then 1 else -1) |> 

  (* Remove distances *)
  List.map (fun (_, p) -> p) 


(* Type of a triangle in a tmesh. Maintains its three points, but also 
 * a mutable list of neighbors that is updated as mesh is built. *)
type triangle = { first: float*float; 
                  second: float*float;
                  third: float*float; 

                  mutable neighbs: triangle list
                }

(* [sos (aka side_overlaps_side) s1 s2] returns true if side s1 overlaps
* side s2, else false. *)
let sos ( (xa, ya), (xb, yb) ) ( (xc, yc), (xd, yd)) =

  (* If both lines are vertical *)
  if (xa=xb) && (xc=xd) then 
    (((min yc yd) > (min yb ya))&&((min yc yd) < (max yb ya))) ||
    (((min yb ya) > (min yc yd))&&((min yb ya) < (max yc yd)))               

  (* Else if only one line is vertical *)
  (* AB side is vert. *)
  else if xa=xb then 
    let m = (yd-.yc) /. (xd -. xc) in 
    let b = yc -. (m *. xc) in 
   
    let (xi, yi) = (xa, m*.xa+.b) in 

    (min xc xd) < xi &&
    (max xc xd) > xi &&
    (min ya yb) < yi &&
    (max ya yb) > yi 
    
  (* CD side is vert. *)
  else if xc=xd then 
    let m = (yb-.ya) /. (xb-.xa) in 
    let b = ya -. (m *. xa) in 

    let (xi, yi) = (xc, m*.xc+.b) in 
    (min xa xb) < xi &&
    (max xa xb) > xi &&
    (min yc yd) < yi &&
    (max yc yd) > yi 

  (* Neither side is vert. *)
  else 
    let m1 = (yb -. ya) /. (xb -. xa) in 
    let b1 = ya -. ( xa *. m1) in 

    let m2 = (yd -. yc) /. (xd -. xc) in 
    let b2 = yc -. ( xc *. m2) in 

    let xi = (b2 -. b1) /. (m1 -. m2) in 
    let yi = m1 *. xi +. b1 in 

    (min xa xb) < xi &&
    (max xa xb) > xi &&
    (min xc xd) < xi &&
    (max xc xd) > xi &&
    (min ya yb) < yi &&
    (max ya yb) > yi &&
    (min yc yd) < yi &&
    (max yc yd) > yi

(* [side_overlaps t s] returns true if new line segment s would 
* intersect triangle t, else false.  *)
let side_overlaps (t:triangle) s = 
  sos (t.first, t.second) s ||
  sos (t.first, t.third) s ||
  sos (t.second, t.third) s 



(* [overlaps_tmesh tmesh s1 s2] Returns true if line segment s1 or s2
* would overlap a triangle in the tmesh. *)
let rec overlaps_tmesh tmesh s1 s2 = match tmesh with 
| [] -> false
| h::t -> (side_overlaps h s1) || (side_overlaps h s2) || 
        (overlaps_tmesh t s1 s2) 

(* [generates_tmesh w h] generates a triangle mesh (tmesh) - a list of 
 * triangles that form a mesh that perfectly covers a plane area between
 * (0, 0) and (w, h). *)
let generate_tmesh w h n = 

  let htbl = Hashtbl.create n in 
  
  (* [gen_tmesh tmesh pts_in pts_out stack] is a recursive helper function. 
   *    - [tmesh : triangle list] tmesh generated so far. 
   *    - [pts] : is a list of points available. 
   *    - [stack : ((float * float), (float * float)) Stack.t] is a stack
   *    maintaining recently added sides to the mesh. *)
  let rec gen_tmesh tmesh points stack = 
    if Stack.is_empty stack then tmesh 

    else 

      let s  =  Stack.pop stack in 
      let pts = order_closest (fst s) (snd s) points in

      let rec process_side c_points = match c_points with 
        (* There is no suitable point to add to the tmesh. *)
        | [] -> try 
                let ptA = List.find (fun x -> not (Hashtbl.mem htbl x)) in
                let ptB = (order_closest ptA ptA points) |> 
                          List.tl |> 
                          List.find (fun x -> not (Hashtbl.mem htbl x) &&
                                              not (overlaps_tmesh tmesh 
                                                  (ptA, ptB) (ptA, ptB) ) in 

                


  let ptA = List.hd rdm_pts in 
  let ptB = List.hd (order_closest ptA ptA rdm_pts) in 
          with 
          | _ -> gen_tmesh tmesh points stack 
       
        (* h might be a suitable poitn. *)
        | h::t -> if h = fst s || h = snd s then process_side t 

          else if overlaps_tmesh tmesh (h, fst(s)) (h, snd(s)) then 
                process_side t else

                let new_t = {
                        first = fst s;
                        second =  snd s;
                        third = h;
                        neighbs = []
              } in 

                (* Add a new triangle to the tmesh, move h to pts_in, the tail
                 * is all the points outside the tmesh, and the stack is the
                 * same *) 
                if (Hashtbl.mem htbl h) && ((Hashtbl.find htbl h) > 2) ||
                   ((Hashtbl.find htbl (fst s)) > 2) then () else

                  (Stack.push (fst s, h) stack;
                   if Hashtbl.mem htbl h 
                     then Hashtbl.add htbl h ((Hashtbl.find htbl h)+1)
                     else Hashtbl.add htbl h 1;
                   Hashtbl.add htbl (fst s) ((Hashtbl.find htbl (fst s)+1)));


                if (Hashtbl.mem htbl h) && ((Hashtbl.find htbl h) > 2) ||
                   ((Hashtbl.find htbl (snd s)) > 2) then () else

                  (Stack.push (snd s, h) stack;
                   if Hashtbl.mem htbl h
                   then Hashtbl.add htbl h ((Hashtbl.find htbl h)+1)
                   else Hashtbl.add htbl h 1;
                   Hashtbl.add htbl (snd s) ((Hashtbl.find htbl (snd s)+1)));

                gen_tmesh (new_t::tmesh) pts stack in 

      process_side pts in 

  let rdm_pts = random_points w h n in 
  let ptA = List.hd rdm_pts in 
  let ptB = List.hd (order_closest ptA ptA rdm_pts) in 
  let st  = Stack.create () in 
 
  Hashtbl.add htbl ptA 1;
  Hashtbl.add htbl ptB 1;

  Stack.push (ptA, ptB) st;

  gen_tmesh [] rdm_pts st


(* [generate_polys w h n] creates [n] different polygons in an area with width
 * [w] and height [h]. These represent the regions the various tribes inhabit.
 *)
let generate_polys w h n  = 

  (* Just create a tmesh for now. *)
  let tm = generate_tmesh w h n in 

  print_endline ("B:"^(string_of_int (List.length tm)));
  tm |> 
  List.map (fun t -> [| t.first; t.second; t.third |] ) |> 
  List.map (fun arr -> Array.map (fun (x,y) -> (int_of_float x, int_of_float y)) arr)



(* [generate_regions w h n] Creates [n] map_regions in an area with width [w]
 * and height [h]. *)
let generate_regions w h n = 
  let gp = generate_polys w h n in

  gp |> List.map (fun p -> {
                        polygon = p;
                        color = random_color ();
                        name = "xyzzy";
                        area = 0.0;
                        neighbors = [];
                        edge_lengths = [];
                      })
let rec main () = 
  let w = generate_regions 500 650 5000 in 

  display {regions = w};
  ignore (read_line() );
  main ()

let () = 
  main ();



