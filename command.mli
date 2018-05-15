(*borrowed from A2*)
(* [command] represents a command input by a player during the game. *)
type command =
 | Step of int
 | ViewAll
 | View of string
 | Save of string
 | Quit

(* [start] represents a command input by a player to start the game. *)
type start =
 | Filename of string
 | Params of int * int * int


(* [parse_game str] is the command that represents player input [str] during the
 * game. If no command is recognized, [str] is assumed to represent "View "[str]
 * requires: [str] is one of the commands forms that a player can use during
 * the game. *)
(* possible commands:
 * "step _" where _ is a non-negative integer, to move that many steps forward
 * in the simulation
 * "quit" to quit the game
 * "save _" where _ is the name of a json file, to save the game state to that file
 * "view all" or "status" to view the information of all the tribes
 * "view _" or "_" where _ is the name of a tribe, to view the information
 * of that tribe *)
val parse_game : string -> command

(* [parse_start str] is the command that represents player input [str] during the game.
 * If no command is recognized, [str] is assumed to represent "Filename "[str]
 * requires: [str] is one of the commands forms that a player can use to start
 * the game. *)
 (* options for params:
  * size _ attitude _ scarceness _
  * s _ a _ s _
  * _ _ _*)
 (* options for filename:
  * file _
  * filename _
  * _*)
val parse_start : string -> start
