open Graphics

let width = 500
let height = 600

type state = string

type map_region = {
  (* Fields relevant for drawing. *)
  polygon : (int * int) array;
  color : int;

  (* Fields providing info to the region. *)
  name : string;
  area : float; 

  (* Relational data derived from map. *)
  neighbors : map_region list;
  edge_lengths : (map_region * float) list;
}


type world_map = {
  (* A list of all the regions in the world. *)
  regions : map_region list
}

(* Displays the world w on an already-open graphics canvas. *)
let rec display_world w = 
  match w with 
  | r::t -> 
    set_color r.color;
    fill_poly r.polygon;
    display_world t

  | [] -> ()

let display w =
  open_graph "";
  resize_window width height;

  display_world w.regions

  
                