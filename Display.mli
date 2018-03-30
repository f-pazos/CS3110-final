(* An abstract type that maintains what the world map actually looks like. 
 *)
type world_map

(* [display state] will create a graphic representation for the current 
   simulation state, so the user can visually see how the world is
   progressing. 
*)
val display : state -> unit
