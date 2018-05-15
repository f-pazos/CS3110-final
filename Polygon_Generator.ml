(* File to play around with how graphics work in OCaml, free of the rest 
 * of the project. *)

open Graphics
open Display

let debug_polygon = false

let random_color () = Random.int 0xFFFFFF

let lat_size = 50

(* [gen_lattice w h size] Generates a starting triangle grid. *)
let gen_lattice w h size =  

  let tmesh = ref [] in 

  for i = 0 to ((w/size)-1) do
    for j = 0 to ((h/size)-1) do 

      let x = size*i in 
      let y = size*j in 

      let x' = min w (size*(i+1)) in 
      let y' = min h (size*(j+1)) in 


      let r = (x+1+(Random.int(size-1)), y+1+(Random.int(size-1))) in 

      tmesh := [| (x,y);   (x',y); r |]::
               [| (x,y);   (x,y'); r |]::
               [| (x',y'); (x,y'); r |]::
               [| (x',y'); (x',y); r |]::(!tmesh) 
      done
    done; 
  
  if debug_polygon then print_endline (string_of_int w) else ();

  (!tmesh)

(* NOTE *)
(* This is an old function that we don't use anymore. Keeping it around just in case. It used
 * to add a bunch of random points within a triangle grid, but we found the benefit to the map 
 * wasn't worth the trade off in performance from the extra step. *)

(* [generates_tmesh w h] generates a triangle mesh (tmesh) - a list of 
 * triangles that form a mesh that perfectly covers a plane area between
 * (0, 0) and (w, h). *)
let generate_tmesh w h n = 

  let remove_triangle tri tris = 
    List.filter (fun t -> not (( tri.(0) = t.(0) ||
                                 tri.(0) = t.(1) || 
                                 tri.(0) = t.(2))&&
                                (tri.(1) = t.(0) || 
                                 tri.(1) = t.(1) || 
                                 tri.(1) = t.(2))&&
                                (tri.(2) = t.(0) || 
                                 tri.(2) = t.(1) ||
                                 tri.(2) = t.(2)))) tris in
 
  (* [area t] returns the area of triangle t. *)
  let area t = 
    let num = fst t.(0) * (snd (t.(1)) - snd (t.(2))) + 
              fst t.(1) * (snd (t.(2)) - snd (t.(0))) + 
              fst t.(2) * (snd (t.(0)) - snd (t.(1))) in 
    float_of_int (abs num) /. 2.0 in       

  (* [gen_tmesh w h n acc] Recursive helper method. Acc is the list of triangles     so far. *)
  let rec gen_tmesh w h n pts acc = 
    if List.length acc < n
    then 
      let t = List.hd acc in 

      let ac = ( float_of_int ( fst t.(2) - fst t.(0) ), 
                 float_of_int ( snd t.(2) - snd t.(0) ) ) in 

      let ab = ( float_of_int ( fst t.(1) - fst t.(0) ),
                 float_of_int ( snd t.(1) - snd t.(0) ) ) in 

      let u = 0.01 +. Random.float 0.98 in 

      let x = (fst t.(0)+int_of_float( u *. (fst ab) +. (1.-.u) *. (fst ac) ), 
               snd t.(0)+int_of_float( u *. (snd ab) +. (1.-.u) *. (snd ac))) in 
      let new_ts = [ [|x; t.(0); t.(1)|];
                     [|x; t.(0); t.(2)|];
                     [|x; t.(1); t.(2)|] ] in 

      let new_acc = List.sort (fun t1 t2 -> if (area t2) > (area t1) then 1
                                       else if (area t1) < (area t2) then -1
                                       else 0) (new_ts @ (remove_triangle t acc)) in 
          
      gen_tmesh w h n (x::pts) new_acc
    else 
      acc in


  let init = gen_lattice w h lat_size in 

  gen_tmesh w h n [] init


(* Here is a type representing the side of a polygon.
 * AF: A side (p0, p1) represents the side of a polygon with points at p0 & p1.
 * RI: None. A side may be given with either point first. They would still be
 * the same side. *)
type side = ( (int*int) * (int*int) ) 

(* Here is a type representing a region, which is basically just the outline of
 * a polygon. 
 *
 * AF: For a given polygon [|p0, p1, ..., pn|], its region is an array of
 * all the edges in the polygon in order - that is, 
 * [| s0, s1, ..., sn |].
 *
 * RI: The relative order of the edges is always maintained. Further, the array 
 * wraps around - that is - the last side connects to the first side. *)
type region = side array 

(* [reg_to_poly reg] Creates a polygon from the region reg. We have to be carefu
 * l to make sure we don't double/skip point when merging since the order of
 * the points in the sides isn't guaranteed. 
 *      Precondition: reg has length over 2. *)
let rec reg_to_poly reg =

  (* We want to make sure we start with the point that is in both the 
   * first and second side of [reg]. *)
  let (pa, pb) = reg.(0) in 
  let (pc, pd) = reg.(1) in 
  let starter = if pa=pc || pa=pd then pa else pb in 

  (* Initialize array *)
  let new_poly = Array.make (Array.length reg) starter in 

  for i = 1 to (Array.length reg) - 1 do
    let (px,py) = reg.(i) in 
    let next_point = if px=new_poly.(i-1) then py else px in 

    new_poly.(i) <- next_point
  done;
  new_poly

(* Prints out region reg labeled with [name]. Only prints if debug is on *)
let print_reg name reg = 
  if debug_polygon then  ( 
  print_string (name ^ ": [|");
  for i = 0 to (Array.length reg)-1 do 
    print_string "( (";
    print_string (string_of_int (i |> Array.get reg |> fst |> fst) ^ ", "); 
    print_string (string_of_int (i |> Array.get reg |> fst |> snd) ^ "), (");
    print_string (string_of_int (i |> Array.get reg |> snd |> fst) ^ ", ");
    print_string (string_of_int (i |> Array.get reg |> snd |> snd) ^ ")); ")
  done;

  print_string "|]";
  print_endline "" )

  else ()


(* Prints out polygon [poly] labeled with [name]. Only prints if debug is on *)
let print_poly name poly = 
  if debug_polygon then (

  print_string (name ^ ": [|");
  for i = 0 to (Array.length poly)-1 do 
    print_string "(";
    print_string (string_of_int (i|> Array.get poly |> fst) ^ ", ");
    print_string (string_of_int (i|> Array.get poly |> snd) ^ "), ");
  done;

  print_string "|]";
  print_endline "")

  else () 


let print_regions regs = 
  if debug_polygon then 
  for i = 0 to (Array.length regs) - 1 do
    print_poly "" regs.(i);
    print_endline ""
  done
  else ()

(* [adj_sides s1 s2] Returns true if s1 and s2 are adjacent sides - that is, 
 * they share a point. *)
let adj_sides (p1, p2)  (p3, p4)  = 
  p1=p3 || p1=p4 || p2=p3 || p2=p4


(* [in_reg s r] Returns true if side [s] is found within region [r] *)
let in_reg ( (p0, p1):side) (r:region) = 
  (* The side might be given with either points first. *)
  Array.mem (p0, p1) r || Array.mem (p1, p0) r 

(* [adj_regs r1 r2] Returns true if r1 and r2 are adjacent regions - that is, 
 * they share at least one edge. 
 * NOTE: This means that two identical regions are considered adjacent.  *)
let adj_regs r1 r2 = 

  (* [Returns true if a side in [sides] is found in region [reg] *)
  let rec contains sides reg i =
    if i >= (Array.length sides) then false else

    let (p0, p1) = sides.(i) in 

    if Array.mem (p0,p1) reg || Array.mem (p1,p0) reg then true
    else contains sides reg (i+1) in 


  contains r1 r2 0 || contains r2 r1 0 


(* [merge r1 r2] Returns a new region that is the merger of regions r1 and r2. 
 * If r1 = r2, then simply returns r1. 
 * Precondition: r1 and r2 are adjacent regions - that is, they share at least one side. *)
let rec merge r1 r2 =
  (* Create a list to hold all the sides.  *)
  let all_sides = ref [] in 

  (*print_reg "r1" r1;  
  print_poly "r1_p" (reg_to_poly r1);
  print_endline ("r1_h: " ^ (string_of_int ((Hashtbl.hash r1))));

  print_reg "r2" r2;
  print_poly "r2_p" (reg_to_poly r2);
  print_endline ("r2_h: " ^ (string_of_int ((Hashtbl.hash r2)) ));*)


  (* Add edges from regions r1 and r2 to the hash table IF a given edge
   * is not in both r1 AND r2. This will get rid of the overlaps. *)
  for i = 0 to (Array.length r1)-1 do 
    let s = r1.(i) in 
    (* If the side isn't in r2, then add it to the hash table. *)
    if not (in_reg s r2) then all_sides := s::(!all_sides)
    else ()
  done; 

  for j = 0 to (Array.length r2)-1 do 
    let s = r2.(j) in 
    (* If the side isn't in r1, add it to the hash tbl. *)
    if not (in_reg s r1) then all_sides := s::(!all_sides)
    else () 
  done; 

  let l = List.length (!all_sides) in

  (* If the list is empty, it's because every element in r1 was in r2 and 
   * vice versa. This means r1=r2, so we can just return r1. *)
  if l=0 then r1 

  (* Otherwise, we need to construct a new region from the sides *)
  else   
    let result = Array.of_list (!all_sides) in

    (* Swaps values of [reg] around so that it meets the representation 
     * invariant for a region. 
     * Precondition: [i] is the index up to which the region has been fixed *)
    let rec fix_reg reg i  = 

      (* If we've fixed the entire array, then we're done. *)
      if i = (Array.length reg) - 1 then ()
      else
        (* Returns the first index j such that j > i and reg.(j) is adjacent
         * to reg.(i). *)
        let rec find_next_index i pos =
          if adj_sides (reg.(i)) (reg.(pos)) then pos 
          else find_next_index i (pos+1) in
        
        let j = find_next_index i (i+1) in 

        (*Swap the next side into place*)
        let temp = reg.(i+1) in 
        reg.(i+1) <- reg.(j);
        reg.(j) <- temp; 

        fix_reg reg (i+1)
    in

    fix_reg result 0;
    (*print_reg "result" result;
    print_endline ("result_h: " ^ (string_of_int (Hashtbl.hash r1)) );*)
    result

(* [cluster rs] Takes an array of regions [rs] and merges adjacent regions
 * until there are n entries in the list. *)
let rec cluster n rs =
  if debug_polygon then print_endline (string_of_int (Array.length rs))
  else ();



  if Array.length rs <= n then rs else 

  (* [find_adj arr] Returns the first index i s.t. the regions at index
   * 0 and i are adjacent. *)
  let rec find_adj i =
    if i >= Array.length rs then (print_endline "larger :/"; i) else
    if adj_regs rs.(0) rs.(i) then i else find_adj (i+1) in 

  (* Find index of region adjacent to rs.(0) *)
  let i = find_adj 1 in 

  (*print_endline "index_found";*)

  (* Create the new region *)
  let new_reg = merge (rs.(0)) (rs.(i)) in    

  (*print_endline "merged";*)

  (* Initialize a new array to copy over to. *)
  let new_arr = Array.make ((Array.length rs)-1) new_reg in 

  let l1 = i-1 in 
  let l2 = (Array.length rs) - i - 1 in 

  Array.blit rs 1 new_arr 0 l1;
  (*print_endline "blit1";*)

  Array.blit rs (i+1) new_arr l1 l2;
  (*print_endline "blit2";*)

  cluster n new_arr

(* [generate_polys w h n] creates [n] different polygons in an area with width
 * [w] and height [h]. These represent the regions the various tribes inhabit.
 *)
let generate_polys w h n  = 

  (* First, create a triangle grid. This has type polygon array.  *)
  let tm = ( gen_lattice w h lat_size)  |>
           Array.of_list |>

           (* Now transform polygon to region *)
           Array.map (fun poly -> 
               (* Create an empty region *)
               let new_reg = Array.make (Array.length poly) ( (0,0),(0,0) ) in 

               (* Create sides from points in polygon *)
               for i = 0 to (Array.length poly)-1 do
                 new_reg.(i)<- (poly.(i), poly.( (i+1) mod (Array.length poly)))
               done;

               (* Return the new region *)
               new_reg) in

  (* Shuffle tm in place so adjacent triangles aren't adjacent in the list. *)
 let result =   for i = (Array.length tm)-1 downto 0 do 
    let j = Random.int (i+1) in 
    let temp = tm.(j) in 

    tm.(j) <- tm.(i);
    tm.(i) <- temp;
  done;

  cluster n tm |> 
  Array.map ( reg_to_poly) in 

  print_regions result; 
  result


