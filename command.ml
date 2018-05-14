(* A type exposed and defined in a .mli file must also be defined in
 * the corresponding .ml file.  So you must repeat the definition
 * of [command] here.  This helps OCaml achieve something called
 * "separate compilation", which you could google for.  Yes,
 * it's a little bit annoying, but there is a good reason for it. *)
 type command =
 | Go of string
 | Take of string
 | Drop of string
 | Quit
 | Look
 | Inventory
 | Score
 | Turns

let parse str =
  let str1 = String.lowercase_ascii str in
  match str1 with
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
         else Go x
