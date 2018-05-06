(* File to play around with how graphics work in OCaml, free of the rest 
 * of the project. *)

open Graphics
open Display

let random_color () = Random.int 0xFFFFFF

let lat_size = 50

(* [gen_lattice w h size] Generates a starting triangle grid. *)
let gen_lattice w h size =  

 
  let tmesh = ref [] in 

  for i = 0 to (w/size) do
    for j = 0 to (h/size) do 

      let x = size*i in 
      let y = size*j in 

      let x' = min w (size*(i+1)) in 
      let y' = min h (size*(j+1)) in 


      let r = (x + (Random.int size), y + (Random.int size) ) in 

      tmesh := [| (x,y);   (x',y); r |]::
               [| (x,y);   (x,y'); r |]::
               [| (x',y'); (x,y'); r |]::
               [| (x',y'); (x',y); r |]::(!tmesh) 
      done
    done; 
   
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



(* [adj_ts t1 t2] returns true if t1 and t2 share exactly one side. Returns
 * false if they are the same triangle. *)
let adj_ts t1 t2 = 

  (* Helper function to sort point arrays consistently. *)
  let f = (fun (x1, y1) (x2, y2) -> 
      if x1 > x2 then 1 else 
      if x2 > x1 then -1 else 
      if y1 > y2 then 1 else
      if y2 > y1 then -1 else 0) in 

  Array.sort f t1;
  Array.sort f t2; 

  (* If same triangle, false. *)
  if t1 = t2 then false else 

  (* Check to see if any two points from t1 are in t2. *)
  (Array.mem t1.(0) t2 && Array.mem t1.(1) t2)||
  (Array.mem t1.(0) t2 && Array.mem t1.(2) t2)||
  (Array.mem t1.(1) t2 && Array.mem t1.(2) t2)

(* [adj_tlist t ts] returns true if t is not in ts and t is adjacent to at
 * to at least one triangle in ts. *)
let rec adj_tlist t ts = 
  Array.exists (fun x -> adj_ts t x) ts

(* [adj_reg r1 r2] Returns true if r1 and r2 are adjacent regions. *)
let rec adj_reg r1 r2 =
  Array.exists (fun x -> adj_tlist x r2) r1

(*

(* [merge r1 r2] Returns a new region that is the merger of regions r1 and r2. 
 * If r1 = r2, then simply returns r1. *)
let rec merge r1 r2 =
  (* Create a hash table. *)
  let tbl = Hashtbl.create (max (Array.length r1) (Array.length r2) ) in 

  (* Add edges from regions r1 and r2 to the hash table IF a given edge
   * is not in both r1 AND r2. This will get rid of the overlaps. *)
  for i = 0 to (Array.length r1) do 
    
  done; 

  for j = 0 to (Array.length r2) do 

  done; *)








(* [cluster ts] Takes an array of regions [ts] and clusters adjacent regions
 * until there are n entries in the list. *)
let rec cluster n ts = 

  print_endline (string_of_int (Array.length ts));
  if Array.length ts <= n then ts else 

  (* [find_adj arr] Returns the first index i s.t. the regions at index
   * 0 and i are adjacent. *)
  let rec find_adj i = 
    if adj_reg ts.(0) ts.(i) then i else find_adj (i+1) in 
    
    let i = find_adj 1 in 
    let new_reg = Array.append (ts.(0)) (ts.(i)) in

      
    let new_arr = Array.make ((Array.length ts)-1) new_reg in 

    let l1 = i-1 in 
    let l2 = (Array.length ts) - i - 1 in 

    Array.blit ts 1 new_arr 0 l1;
    Array.blit ts (i+1) new_arr l1 l2;

    cluster n new_arr

(* [generate_polys w h n] creates [n] different polygons in an area with width
 * [w] and height [h]. These represent the regions the various tribes inhabit.
 *)
let generate_polys w h n  = 

  (* First, create a triangle grid *)
  let tm = ( gen_lattice w h lat_size)  |>
           List.map (fun x -> [|x|]) |>
           Array.of_list in

  (* Shuffle tm in place so adjacent triangles aren't adjacent in the list. *)
  for i = (Array.length tm)-1 downto 0 do 
    let j = Random.int (i+1) in 
    let temp = tm.(j) in 

    tm.(j) <- tm.(i);
    tm.(i) <- temp;
  done;
  print_endline "shuffled";
  cluster n tm




(* [generate_regions w h n] Creates [n] map_regions in an area with width [w]
 * and height [h]. *)
let generate_regions w h n = 
  let gp = generate_polys w h n in

  gp |> Array.map (fun p -> {
                        polygons = p;
                        color = random_color ();
                        name = "xyzzy";
                        area = 0.0;
                        neighbors = [];
                        edge_lengths = [];
                      }) |> Array.to_list
let rec main () = 

  let tock = Sys.time () in 
  let w = generate_regions 1920 1080 50 in 
  print_endline (string_of_float ( (Sys.time ()) -. tock));

  display {regions = w};
  ignore (read_line() );
  main ()

(* Commented out for prototype submission. This is how we've been testing our map generator. *)
let () = 
  main ();



