open State
open Generator
open Display
open Command

(* prints that there is a problem with the state file and exits*)
let file_problem_exit () =
  print_string "Sorry, there was a problem with the state file. Exiting the game.";
  exit 0

(* prints that there is a problem with the game engine and exits*)
let game_problem_exit () =
  print_string "Sorry, there was a problem with the game engine. Exiting the game.";
  exit 0

let rec print_opinions op_list =
  match op_list with
  | [] -> ()
  | (s,i)::t -> print_endline s^": "^(string_of_int i); print_opinions t

let rec print_neighbors neighbor_list =
match op_list with
| [] -> ()
| (s,f)::t -> print_endline s^": "^(string_of_float f); print_neighbors t

(* [print_tribe s t_str] prints the details of tribe with name [t_str] and its
 * region in state [s]*)
let print_tribe s t_str =
  let t = try List.assoc t_str s.tribes
    with not_found -> print_endline "Tribe "^t_str^" not found." in
  begin
    print_endline "Name: "^t.name;
    print_endline "Population: "^(string_of_int t.pop);
    print_endline "Food: "^(string_of_int t.pop);
    print_endline "Tools: "^(string_of_int t.pop);
    print_endline "Weapons: "^(string_of_int t.pop);
    print_endline "Attitude: "^(string_of_int t.pop);
    print_endline "Opinions: ";
    print_opinions t.opins;
    let r_str = t.reg in
    let r = try List.assoc r_str s.regions
      with not_found -> print_endline "Region "^r_str^" not found." in (*shouldn't happen*)
    begin
      print_endline "Region name: "^r.name;
      print_endline "Area: "^(string_of_int r.area);
      print_endline "Climate: "^(string_of_float r.climate);
      print_endline "Neighbors: ";
      print_neighbors r.neighbors
    end
  end

(* [print_tribe s] prints the details of every tribe/region in state s*)
let print_state s =
  let print_tribes_remaining t_list =
  match t_list with
  | [] -> ()
  | (name, tribe)::tl -> print_tribe s name; print_tribes_remaining tl
  in print_tribes_remaining s.tribes

(* [play_game s] starts a game with the state s, and uses a REPL to
 * prompt the user for the number of steps the game should take *)
let rec play_game s =
  (*TODO: map/display parts*)
  print_endline "Please enter a command.\n";
  print_string "> ";
  let player_command = match read_line () with
  | exception End_of_file -> game_problem_exit ()
  | s -> Command.parse_game s
  in try
  match player_command with
  | Step x -> let s_new = State.step s x in play_game s_new
  | ViewAll -> print_state s
  | View x -> print_tribe s x
  | Save x -> failwith "Undefined"
  | Quit -> failwith "Undefined"
  with _ -> game_problem_exit ()

let create_state start_command = match start_command with
  | Filename f ->
    begin
      let s = try load_state f with _ -> file_problem_exit () in
      play_game s
    end
  | Params (size, attitude, scarceness) ->
    begin
      let s = try generate_state size attitude scarceness with _ -> game_problem_exit () in
      play_game s
    end


let main () =
  ANSITerminal.(print_string [red]
    "\n\nWelcome to Society Survival, a CS 3110 game.\n");
  print_endline "Please enter the name of the game file you want to load,";
  print_endline "or enter the parameters for a new game as";
  print_endline "size _ attitude _ scarceness _";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | str -> play_game (create_state (parse_start str))

let () = main ()

(*copied from Sourabh's A2 implementation-------------------------------------*)

(* prints the items and descriptions in i *)
let rec print_items (i : (string * string) list) =
  match i with
  | [] -> print_string ""
  | (a,b)::tl -> print_endline (a^", which is "^b^"."); print_items tl

(* prints the items in i *)
let rec print_inv (i : string list) =
  match i with
  | [] -> print_endline ""
  | [a] -> print_endline (a^".")
  | hd::tl -> print_string (hd^", "); print_inv tl

let rec advance_state my_state = let items = State.items_and_descriptions my_state in
  let description = try State.current_description my_state
    with AdventureFileException -> adventure_problem_exit () in
  print_endline description;
  print_endline "The items in this room are: ";
  print_items items;
  if score my_state >= win_score my_state
  then print_endline (State.win_message my_state) else ();
  print_endline "Please enter a command.\n";
  print_string "> ";
  let player_command = match read_line () with
  | exception End_of_file -> game_problem_exit ()
  | s -> Command.parse s
  in try
  match player_command with
  | Quit -> print_endline "Bye!"; exit 0
  | Look -> begin print_endline description;
            print_endline "The items in this room are: ";
            print_items items;
            advance_state (State.do' player_command my_state)
            end
  | Inventory -> begin print_inv (State.inv my_state);
                 advance_state (State.do' player_command my_state)
                 end
  | Score -> begin print_int (State.score my_state);
             print_endline "";
             advance_state (State.do' player_command my_state)
             end
  | Turns -> begin print_int (State.turns my_state);
             print_endline "";
             advance_state (State.do' player_command my_state)
             end
  | Go x -> begin let new_state =
            try (State.do' player_command my_state) with
            | AdventureFileException -> adventure_problem_exit ()
            | InputException -> print_endline
              "The direction specified is not a valid direction."; my_state
            | ExitException -> print_endline
              "This exit is not reachable. You do not have the right keys."; my_state
            in advance_state new_state
            end
  | Take x -> begin let new_state =
              try (State.do' player_command my_state) with
              | AdventureFileException -> adventure_problem_exit ()
              | ItemException -> print_endline
                "There is no such item in the current room."; my_state
              in advance_state new_state
              end
  | Drop x -> begin let new_state =
              try (State.do' player_command my_state) with
              | AdventureFileException -> adventure_problem_exit ()
              | ItemException -> print_endline
                "There is no such item in the inventory."; my_state
              in advance_state new_state
              end
  with _ -> adventure_problem_exit ()

(* [play_game f] plays the game in adventure file [f]. *)
let play_game f =
  let j = try Yojson.Basic.from_file f with _ -> `Null in
  let state1 = try State.init_state j with _ -> adventure_problem_exit () in
  advance_state state1

(*exit: print_string s; exit 0*)

(* [main ()] starts the REPL, which prompts for a game to play.
 * You are welcome to improve the user interface, but it must
 * still prompt for a game to play rather than hardcode a game file. *)
let main () =
  ANSITerminal.(print_string [red]
    "\n\nWelcome to the 3110 Text Adventure Game engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

let () = main ()
