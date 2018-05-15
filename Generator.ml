open State
open Graphics

(*****************************************************************************)
(* POINT and related functions                                               *)
(*****************************************************************************)
type point = int * int

(* [distance p0 p1] returns the cartesian distance between p0 and p1. *)
let distance (x0, y0) (x1, y1) : float = ( (float_of_int (x0-x1))**2.0 +. (float_of_int(y0-y1))**2.0)**0.5


(*****************************************************************************)
(* SIDE and related functions                                                *)
(*****************************************************************************)
type side = point * point 

(* [adj_sides s1 s2] Returns true if s1 and s2 are adjacent sides - that is, 
 * they share a point. *)
let adj_sides (p1, p2)  (p3, p4)  = 
  p1=p3 || p1=p4 || p2=p3 || p2=p4

(*****************************************************************************)
(* OUTLINE and related functions                                             *)
(*****************************************************************************)
type outline = {size: int; points : point array; sides : side array}

(* TODO *)
(* [neighbors o1 o2] Returns true if outlines [o1] and [o2] share at least
 * one side, but not all sides.  *)
let neighbors o1 o2 = true

(*****************************************************************************)
(* WORLD and related functions                                               *)
(*****************************************************************************)
type world = {size: int; border: outline; regions: outline array} 

(* TODO 0 : rep_ok's *)
let repok_outline out = ()
let repok_world w = ()

(*TODO 1 : finish implementing [outline_of_polygon]. *)
(* [polygon_of_outline out] creates a Graphics.polygon out of a given outline *)
let polygon_of_outline out = out.points

(* [outline_of_polygon poly] creates an outline of the Graphics.polygon poly. *)
let outline_of_polygon poly = {size = 0; points = [||]; sides = [||] }

(*TODO : test [area_of_outline] and [len_border] *)
(* [area_of_outline o] returns the area of outline o. *) 
(* Based on this method: https://web.archive.org/web/20100405070507/http://valis.cs.uiuc.edu/~sariel/research/CG/compgeom/msg00831.html *)
let area_of_outline (o:outline) : float = 
  (* This method essentially works via Green's theorem like a 
   * planometer does. *)
  let n = o.size in 
  let area = ref 0.0 in 
  for i = 0 to n-1 do
    let j = (i+1) mod n in
    area := (!area) +. float_of_int (fst (o.points.(i)) * snd (o.points.(j)));
    area := (!area) -. float_of_int (snd (o.points.(i)) * fst (o.points.(j)));
  done;
  
  area := (!area) /. 2.0;
  if (!area) < 0.0 then area := -1.0 *. (!area) else ();

  !area

(* [len_border out1 out2] returns the length of the border shared between
 * outlines [out1] and [out2]. *)
let len_border (out1:outline) (out2:outline) : float = 
  let shared_sides = Hashtbl.create (max out1.size out2.size) in 

  (* Add a side if it's in out1 AND out2. *)
  for i=0 to out1.size - 1 do
    let (p0, p1) = out1.sides.(i) in 
    if Array.mem (p0, p1) out2.sides ||
       Array.mem (p1, p0) out2.sides
    then Hashtbl.add shared_sides (p0, p1) (p0,p1) else ()
  done;

  Hashtbl.fold (fun (a,b) _ acc -> acc +. (distance a b)) shared_sides 0.0



(*TODO : Implement names from text file here.*) 
(* [generate_names n] randomly generates a list of [n] strings representing
 * the names of the regions *)
let rec generate_names n : string array = 
  let arr = Array.make n "" in 
  for i = 0 to n-1 do 
    arr.(i) <- string_of_int n
  done;
  arr

(* TODO : test *)
(* [generate_neighbors out regs] finds the ids of all of out's neighbors. 
 * Returns them in a string list. 
 * - [names] : Association list of type (outline * string) list that 
 *    holds the various names of regions found in [map].
 * - [outline] : The outline that we want to find neighbors of. 
 * - [world] : The world object that outline exists in.  *)
let generate_neighbors names outline m : (string*float) list =

  (* First, find all neighbors *)
  let neighbs = ref [] in 

  (* First, create an assoc list from the names to the outlines
   * themselves. *)
  for i = 0 to m.size - 1 do 
    let r = m.regions.(i) in 
    if neighbors outline r 
    then neighbs := (names.(i), r)::(!neighbs)
  done;
  (* Now, map each r to the length it shares with 
   * [outline]. *)
  List.map (fun (n, r) -> (n, len_border outline r)) (!neighbs)


(* [generate_map w h n] Creates a map with outline that is the rectangle from 
 * (0,0) to (w,h) with [n] disctint regions *)
let generate_map w h n  = failwith "Unimplemented"


(*TODO : test *)
(* [generate_regions n] Creates the regions object for the initial state. *)
let generate_regions n w h : (string * State.region) list = 
  (* Generate a map *)
  let m = generate_map n w h in

  (* Generate a bunch of names *)
  let names = generate_names n in 
 
  (* Empty list that will hold all regions *)
  let regs = ref [] in

  (* For every outline found in m.regions, create State.region object *)
  for i=0 to m.size-1 do 
    let o = m.regions.(i) in 
    regs := ( names.(i), 
              { name = names.(i);
              area = int_of_float (area_of_outline o);
              climate = Random.float 10.0; (*TODO balance this*)
              neighbors = generate_neighbors names o m; 
              polygon = o.points })::(!regs) 
  done;
  (!regs)


(* [generate_tribes regs] Creates the tribes for a given map regs. *)
let generate_tribes regs  = []

(* TODO : fix this *)
let generate_state size attitude scarceness = failwith "Unimplemented"

let save_state file st = failwith "Uniplemented"

let load_state file = failwith "Unimplemented"
