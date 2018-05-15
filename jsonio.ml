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

(* [json_of_region r] creates a json object representing [r] *)
let json_of_region (r : region) : Yojson.Basic.json =
  let name = `String r.name in
  let area = `Int r.area in
  let climate = `Float r.climate in
  let neighbors = `List (List.map (fun (name,prox) -> `Assoc [("name", `String name);
    ("prox", `Float prox)]) r.neighbors) in
  let polygon_list = Array.to_list r.polygon in
  let polygon = `List (List.map (fun (x,y) -> `Assoc [("x", `Int x);
    ("y", `Int y)]) polygon_list) in
  let base_color = `Int r.base_color in
  `Assoc [
  ("name", name);
  ("area", area);
  ("climate", climate);
  ("neighbors", neighbors);
  ("polygon", polygon);
  ("base_color", base_color);
  ]

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

(* [json_of_tribe t] creates a json object representing [t] *)
let json_of_tribe (t : tribe) : Yojson.Basic.json =
  let name = `String t.name in
  let pop = `Int t.pop in
  let food = `Int t.food in
  let tools = `Int t.tools in
  let weps = `Int t.weps in
  let attd = `Int (int_of_attd t.attd) in
  let opins = `List (List.map (fun (name,op) -> `Assoc [("name", `String name);
    ("op", `Int op)]) t.opins) in
  let reg = `String t.reg in
  `Assoc [
  ("name", name);
  ("pop", pop);
  ("food", food);
  ("tools", tools);
  ("weps", weps);
  ("attd", attd);
  ("opins", opins);
  ("reg", reg);
  ]

(* [state_of_json j] creates a state object from the JSON object [j] that
   represents a state *)
let state_of_json j = {
  regions = pair_map to_string "name" region_of_json "region" (to_list (member "regions" j));
  tribes = pair_map to_string "name" tribe_of_json "tribe" (to_list (member "tribe" j));
}

(* [json_of_state s] creates a json object representing [s] *)
let json_of_state (s:state) : Yojson.Basic.json =
  let regions_json = `List (List.map (fun (name, r) -> `Assoc [("name", `String name);
    ("region", json_of_region r)]) s.regions) in
  let tribes_json = `List (List.map (fun (name, t) -> `Assoc [("name", `String name);
    ("tribe", json_of_tribe t)]) s.tribes) in
  `Assoc [("regions", regions_json); ("tribes", tribes_json)]

let read_state filename =
  let j = try Yojson.Basic.from_file filename with _ -> failwith "file error" in
  try state_of_json j with _ -> failwith "file error"

let write_state filename s =
  Yojson.Basic.to_file filename (json_of_state s)
