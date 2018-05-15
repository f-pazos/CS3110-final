open Yojson.Basic.Util
open State

let pair_map f1 f1_name f2 f2_name lst =
  List.map (fun j -> (f1 (member f1_name j), f2 (member f2_name j))) lst

let attd_of_int x = match x with
  | 0 -> Generous
  | 1 -> Neutral
  | 2 -> Aggressive
  | _ -> raise TypeError

let int_of_attd x = match x with
  | Generous -> 0
  | Neutral -> 1
  | Aggressive -> 2

let region_of_json j = {
  name = to_string (member "name" j);
  area = to_int (member "area" j);
  climate = to_number (member "climate" j);
  neighbors = pair_map to_string "name" to_number "prox" (to_list (member "neighbors" j));
  polygon = pair_map to_int "x" to_int "y" (to_list (member "polygon" j));
  base_color = to_int (member "base_color" j);
  }

let tribe_of_json j = {
  name = to_string (member "name" j);
  pop = to_int (member "pop" j);
  food = to_int (member "food" j);
  tools = to_int (member "tools" j);
  weps = to_int (member "weps" j);
  attd = attd_of_int to_int (member "attd" j);
  opins = pair_map to_string "name" to_int "op" (to_list (member "opins" j));
  reg = to_string (member "reg" j);
  }

let state_of_json j = {
  regions = pair_map to_string "name" region_of_json "region" (to_list (member "regions" j));
  tribes = pair_map to_string "name" tribe_of_json "tribe" (to_list (member "tribe" j));
}

let read_state filename =
  let j = try Yojson.Basic.from_file filename with _ -> FileError in
  try state_of_json j with _ -> FileError

let write_state filename s = failwith "Undefined"
