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
val parse_game : string -> command

(* [parse_start str] is the command that represents player input [str] during the game.
 * If no command is recognized, [str] is assumed to represent "Filename "[str]
 * requires: [str] is one of the commands forms that a player can use to start
 * the game. *)
val parse_start : string -> start
