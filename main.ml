open State
open Generator
open Jsonio
open Display
open Command

(* [file_problem_exit ()] prints that there is a problem with the state file
 * and exits*)
let file_problem_exit () =
  print_string "Sorry, there was a problem with the state file. Exiting the game.";
  exit 0

(* [game_problem_exit ()] prints that there is a problem with the game engine
 * and exits*)
let game_problem_exit () =
  print_string "Sorry, there was a problem with the game engine. Exiting the game.";
  exit 0

(* [regular_exit ()] exits the game normally*)
let regular_exit () =
  print_string "Bye!";
  exit 0

(* [print_opinions op_list] prints all the tribe names and opinions in [op_list]*)
let rec print_opinions op_list =
  match op_list with
  | [] -> ()
  | (s,i)::t -> print_endline (s^": "^(string_of_int i)); print_opinions t

(* [print_neighbors neighbor_list] prints all the region names and border edge
 * lengths in [neighbor_list]*)
let rec print_neighbors neighbor_list =
match neighbor_list with
| [] -> ()
| (s,f)::t -> print_endline (s^": "^(string_of_float f)); print_neighbors t

(* [print_tribe s r_str] prints the details of region with name [r_str] in state [s]*)
let print_region (s : state) r_str =
  try let r = List.assoc r_str s.regions in (*shouldn't happen*)
  begin
    print_endline ("Region name: "^r.name);
    print_endline ("Area: "^(string_of_int r.area));
    print_endline ("Climate: "^(string_of_float r.climate));
    print_endline "Neighbors: ";
    print_neighbors r.neighbors
  end
  with not_found -> print_endline ("Region "^r_str^" not found.")

(* [print_tribe s t_str] prints the details of tribe with name [t_str] and its
 * region in state [s]*)
let print_tribe (s : state) t_str =
  let tribes_lower = List.map (fun (a, b) -> (String.lowercase_ascii a, b)) s.tribes in
  try let t = List.assoc t_str tribes_lower in
  begin
    print_endline ("Name: "^t.name);
    print_endline ("Population: "^(string_of_int t.pop));
    print_endline ("Food: "^(string_of_int t.pop));
    print_endline ("Tools: "^(string_of_int t.pop));
    print_endline ("Weapons: "^(string_of_int t.pop));
    print_endline ("Attitude: "^(string_of_int t.pop));
    print_endline "Opinions: ";
    print_opinions t.opins;
    let r_str = t.reg in
    print_region s r_str
  end
  with not_found -> print_endline ("Tribe "^t_str^" not found.")

(* [print_tribe s] prints the details of every tribe/region in state s*)
let print_state s =
  let rec print_tribes_remaining t_list =
  match t_list with
  | [] -> ()
  | (name, tribe)::tl -> print_tribe s name; print_tribes_remaining tl
  in print_tribes_remaining s.tribes

let rec play_game s =
  (*TODO: map/display parts*)
  print_endline "Please enter a command.\n";
  print_string "> ";
  let player_command = match read_line () with
  | exception End_of_file -> game_problem_exit ()
  | s -> parse_game s
  in try
  match player_command with
  | Step x -> let s_new = State.step s x in play_game s_new
  | ViewAll -> print_state s; play_game s
  | View x -> print_tribe s x; play_game s
  | Save x ->
    begin
      try write_state x s
      with _ (*FileError*) -> print_endline "Failed to save the state.";
      play_game s
    end
  | Quit -> regular_exit ()
  with _ -> game_problem_exit ()

let start_game start_command = match start_command with
  | Filename f ->
    begin
      let s = try read_state f with _ -> file_problem_exit () in
      play_game s
    end
  | Params (size, attitude, scarceness) ->
    begin
      let s = try generate_state size attitude scarceness
      with _ -> game_problem_exit () in
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
  | str -> start_game (parse_start str)

let () = main ()
