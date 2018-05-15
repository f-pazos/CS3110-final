open State
open Graphics
open Polygon_Generator

let debug_generator = false


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

(* TODO : test*)
(* [neighbors o1 o2] Returns true if outlines [o1] and [o2] share at least
 * one side, but not all sides.  *)
let neighbors o1 o2 =
  if o1 = o2 then false else

  let is_neighb = ref false in

  for i = 0 to o1.size - 1 do
    (* Check if the side is in o2. *)
    let (p0, p1) = o1.sides.(i) in
    if Array.mem (p0, p1) o2.sides ||
       Array.mem (p1, p0) o2.sides
    then
      is_neighb := true
    else
      ()
  done;

  for i = 0 to o2.size - 1 do
    (* Check if the side is in o1. *)
    let (p1, p0) = o2.sides.(i) in
    if Array.mem (p0, p1) o1.sides ||
       Array.mem (p1, p0) o1.sides
    then
      is_neighb := true
    else
      ()
  done;

  (!is_neighb)

(*****************************************************************************)
(* WORLD and related functions                                               *)
(*****************************************************************************)
type world = {size: int; regions: outline array}

(* TODO 0 : rep_ok's *)
let repok_outline out = ()
let repok_world w = ()

(*TODO t : test *)
(* [polygon_of_outline out] creates a Graphics.polygon out of a given outline *)
let polygon_of_outline out = out.points


(* [outline_of_polygon poly] creates an outline of the Graphics.polygon poly. *)
let outline_of_polygon poly =

  let n = Array.length poly in
  let new_sides = Array.make n ( (0,0), (0,0) ) in

  for i=0 to n-1 do
    new_sides.(i) <- (poly.(i), poly.((i+1) mod n ))
  done;


  {size = Array.length poly;
   points = poly;
   sides = new_sides}

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



(*TODO : Test a few times. *)
(* [generate_names n] randomly generates a list of [n] strings representing
 * the names of the regions *)
let rec generate_names n : string array =

  (* Reads in names from "tribe_names.txt" and creates a list of strings. *)
  let rec read_names ic acc =
    try
      let new_line = input_line ic in
      read_names ic (new_line::acc)
    with
        _ -> acc in

  try
    let ic = open_in "tribe_names.txt" in
    let names = read_names ic [] |> (List.map String.trim) |> Array.of_list in 


    let result = Array.make n "" in

    (* Randomly choose n strings from [names] *)
    for i = 0 to n-1 do
      result.(i) <- names.(Random.int (Array.length names))
    done;

    (* We need to check that there are n unique names *)
    let tbl = Hashtbl.create n in
    for i = 0 to n-1 do
      Hashtbl.add tbl result.(i) result.(i)
    done;

    (* If the hashtbl is not of size n, then that means we accidentally
     * grabbed two of the same name. In this case, we should regenerate *)
    if Hashtbl.length tbl <> n then generate_names n else result

  with _ -> failwith "Error reading tribe_names.txt"


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
let generate_map w h n  =

  (* Create the outlines from polygons *)
  let outlines = generate_polys w h n |> Array.map (outline_of_polygon) in

  if debug_generator then print_endline (string_of_int (Array.length outlines))
  else ();
  {size = Array.length outlines; regions = outlines}



(* [generate_color ()] Returns a random color for a region *)
let generate_color () =

  (* Each channel is at least [a]. This ensures the color is light enough
   * to read easily *)
  let a = 100 in

  let red = a + Random.int (256-a) in
  let green = a + Random.int (256-a) in
  let blue = a + Random.int (256-a)in

  blue + green*256 + red *256*256


(* [generate_regions n] Creates the regions object for the initial state. *)
let generate_regions w h n scar : (string * State.region) list = 
  (* Generate a map *)
  let m = generate_map w h n in

  (* Generate a bunch of names *)
  let names = generate_names n in

  (* Empty list that will hold all regions *)
  let regs = ref [] in

  (* For every outline found in m.regions, create State.region object *)
  for i=0 to m.size-1 do
    let o = m.regions.(i) in
    regs := ( names.(i),
              { name = names.(i);
              area = int_of_float ((area_of_outline o)/.100.);
              climate = (float(scar)/.10.)+.0.8; (*TODO balance this*)
              neighbors = generate_neighbors names o m; 
              polygon = o.points;
              base_color = generate_color ()
              })::(!regs)
  done;
  (!regs)

(* [generate_opins nbrs] returns a [(string * int) list] representing
 * the [nbrs] with the opinion set to 0 *)
let rec generate_opins nbrs =
  match nbrs with
  | [] -> []
  | (id,_)::tl -> (id,0)::(generate_opins tl)

(* [generate_tribes regs] Creates the tribes for a given map regs. *)
let rec generate_tribes regs attd scr =
  match regs with
  | [] -> []
  | (id,r)::tl -> begin
    let popn = (Random.int 100) + 20 in
    let rand = Random.int 100 in
    let attd_ = begin
      if rand < 20 then (attd - 1)
      else if rand > 80 then (attd + 1)
      else attd
      end in
    (id,{
      name = id;
      pop = popn;
      food = truncate (r.climate *. float(popn));
      tools = 0;
      weps = 0;
      attd = begin
        if ((attd_ = 0) || (attd_ = 3)) then Generous
        else if (attd_ = 1) then Neutral
        else Aggressive
      end;
      opins = generate_opins r.neighbors;
      reg = id;
      last_action = Food;
    })::(generate_tribes tl attd scr)
  end

(* [generate_state size attitude scarceness] Generates a starting state. *)
let generate_state size attitude (scarceness:int) = 
  let regions_ = generate_regions 800 800 size scarceness in
  let st0 = { 
    regions = regions_;
    turns = 0;
    tribes = generate_tribes regions_ attitude scarceness
  } in


  (* TODO remove in final version *)
  let rec print_neighbors n = match n with
    | [] -> ()
    | (n, f)::t -> print_endline ("\t" ^ n ^ ", " ^ (string_of_float f)) ;
                   print_neighbors t in

  let rec print_regs regs = match regs with
    | [] -> ()
    | (n, r)::t ->
      print_endline (n ^ ": " ^ (string_of_int r.area));
      print_neighbors r.neighbors;
      print_endline "";
      print_regs t in

  if debug_generator then print_regs st0.regions else ();
  st0
