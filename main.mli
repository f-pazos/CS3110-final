open State

(* [play_game s] starts a game with the state s, and uses a REPL to
 * prompt the user for the number of steps the game should take *)
val play_game : state -> unit

(* [main] starts a REPL that prompts the user for a state file or
 * paramters for starting the game, and then starts the game *)
val main : unit -> unit
