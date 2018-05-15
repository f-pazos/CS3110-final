(* TODO Implement state so this can be removed. *)
open State


(* Initializes values for graphics usage *)
val display_init : unit -> unit 


(* [display world_map] will create a graphic representation for the current
 * simulation state. *)
val display : state -> unit
