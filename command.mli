(* [command] represents a command input by a player. *)
(* You may redefine [command] to be synonymous with whatever
 * type you wish, but its name must not be changed.
 * It is okay to expose this type rather than make it abstract,
 * because it is not the representation type of a data structure. *)
type command =
| Go of string
| Take of string
| Drop of string
| Quit
| Look
| Inventory
| Score
| Turns


(********************************************************
 * DO NOT CHANGE THIS CODE
 * It is part of the interface the course staff will
 * use to test your submission.
 *)

(* [parse str] is the command that represents player input [str].
 * If no command is recognized, [str] is assumed to represent "Go "[str]
 * requires: [str] is one of the commands forms described in the
 *   assignment writeup. *)
val parse : string -> command

(* END DO NOT CHANGE
 ********************************************************)

(* You are free to add more code below *)
