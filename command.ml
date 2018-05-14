type command =
 | Step of int
 | ViewAll
 | View of string
 | Save of string
 | Quit

type start =
 | Filename of string
 | Params of int * int * int

let parse_start str =
  let str1 = String.lowercase_ascii str in failwith "Undefined"

  Str.string_match (Str.regexp "size \\([0-9]\\)+") input1 0
  Str.matched_string input1

  
  (* options for params:
   * size _ attitude _ scarceness _
   * s _ a _ s _
   * _ _ _*)
  (* options for filename:
   * file _
   * filename _
   * _*)

  (* match str1 with
  | "quit" -> Quit
  | "look" -> Look
  | "inventory" -> Inventory
  | "inv" -> Inventory
  | "score" -> Score
  | "turns" -> Turns
  | x -> if String.length x < 2 || String.sub x 0 2 = "go"
           then Go (String.sub x 3 (String.length x - 3))
         else if String.length x >= 5 then
                let the_item = String.sub x 5 (String.length x - 5) in
                if String.sub x 0 5 = "take " then Take the_item else
                if String.sub x 0 5 = "drop " then Drop the_item else Go x
         else Go x *)

let parse_game str = failwith "Undefined"
