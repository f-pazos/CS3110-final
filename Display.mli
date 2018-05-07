(* TODO Implement state so this can be removed. *)
type state = string

(* A type that maintains data intrinsic to a certain region - area, climate, 
 * color, etc. *)

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

(* An abstract type that holds what the map of the world looks like. This has
 * a physical map, as well as various details about the regions themselves
 * size, shared border length, etc. 
 *)
type world_map = {
  (* A list of all the regions in the world. *)
  regions : map_region list
}


(* TODO change this to display a state. *)
(* [display world_map] will create a graphic representation for the current 
   simulation state, so the user can visually see how the world is
   progressing. 
*)
val display : world_map -> unit
