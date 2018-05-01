(* note: I'm just copy-pasting the mli files: it might be possible to just compile
 * all the ml files and use that to check types
 * Also, I'm not sure if this file is even necessary*)
open State

module type DISPLAY = sig
  (* TODO Implement state so this can be removed. *)
  type state = string

  type map_region = {
    polygon : (int * int) array;
    color : int;

    name : string;
    area : float;

    neighbors : map_region list;
    edge_lengths : (map_region * float) list;
  }

  type world_map = {
    regions : map_region list
  }

  val display : world_map -> unit

end (*Display*)

module type GENERATOR = sig
  val generate_state : int -> int -> int -> state
  val save_state : string -> state -> unit
  val load_state : string -> state
end (*Generator*)

module type MAIN = sig
  val play_game : state -> unit
  val main : unit -> unit
end (*main*)

module type STATE = sig
  type region = {
    name : string;
    area : int;
    climate : float;
    neighbors : (string * float) list;
  }

  type attd = Generous | Neutral | Agressive

  type tribe = {
    name : string;
    pop : int;
    food : int;
    tools : int;
    weps : int;
    attd : attd;
    opins : (string * int) list;
    reg : string;
  }
  type state = {
    regions : (string * region) list;
    tribes : (string * tribe) list;
  }

  type action = Food | Tools | Weapons | Attack of string | Gift of (string * int)

  val decide : state -> string -> action
  val do_action : state -> string -> action -> state
  val metabolize : tribe -> tribe
  (*TODO*)
  (* val step : state -> int -> state *)

end (*state*)

module CheckDisplay : DISPLAY = Display
(* module CheckGenerator : GENERATOR = Generator
module CheckMain : MAIN = Main *)
module CheckState : STATE = State
