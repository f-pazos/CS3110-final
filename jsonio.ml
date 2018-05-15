open Yojson.Basic.Util
open State

(* [pair_map lst] is an association list generated from [lst], where
   each element of [lst] is a json object with two fields [f1_name]
   and [f2_name], and [f1] (member [f1_name] j) and [f2] (member [f2_name] j)
   are the items that in [lst] are represented in json form*)
let pair_map f1 f1_name f2 f2_name lst =
  List.map (fun j -> (f1 (member f1_name j), f2 (member f2_name j))) lst

(* [attd_of_int x] is the attd represented by [x]*)
let attd_of_int x = match x with
  | 0 -> Generous
  | 1 -> Neutral
  | 2 -> Aggressive
  | _ -> failwith "type error"

(* [int_of_attd x] is the int represented by [x]*)
let int_of_attd x = match x with
  | Generous -> 0
  | Neutral -> 1
  | Aggressive -> 2

(* [region_of_json j] creates a region object from the JSON object [j] that
   represents a region *)
let region_of_json j = {
  name = to_string (member "name" j);
  area = to_int (member "area" j);
  climate = to_number (member "climate" j);
  neighbors = pair_map to_string "name" to_number "prox" (to_list (member "neighbors" j));
  polygon = Array.of_list (pair_map to_int "x" to_int "y" (to_list (member "polygon" j)));
  base_color = to_int (member "base_color" j);
  }

(* [tribe_of_json j] creates a tribe object from the JSON object [j] that
   represents a tribe *)
let tribe_of_json j = {
  name = to_string (member "name" j);
  pop = to_int (member "pop" j);
  food = to_int (member "food" j);
  tools = to_int (member "tools" j);
  weps = to_int (member "weps" j);
  attd = attd_of_int (to_int (member "attd" j));
  opins = pair_map to_string "name" to_int "op" (to_list (member "opins" j));
  reg = to_string (member "reg" j);
  }

(* [state_of_json j] creates a state object from the JSON object [j] that
   represents a state *)
let state_of_json j = {
  regions = pair_map to_string "name" region_of_json "region" (to_list (member "regions" j));
  tribes = pair_map to_string "name" tribe_of_json "tribe" (to_list (member "tribe" j));
}

let read_state filename =
  let j = try Yojson.Basic.from_file filename with _ -> failwith "file error" in
  try state_of_json j with _ -> failwith "file error"

let write_state filename s = failwith "Undefined"
