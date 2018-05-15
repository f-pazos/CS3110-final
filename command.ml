type command =
 | Step of int
 | ViewAll
 | View of string
 | Save of string
 | Quit

type start =
 | Filename of string
 | Params of int * int * int

exception Parsing_Error

(* [parse_params_full str] is the command that represents player input [str]
 * when starting the game, specifically when generating a new state using
 * size, attitude, and scarceness parameters.
 * requires: [str] is of the form "size _ attitude _ scarceness _", where each
 * _ is a non-negative integer.
 * raises Parsing_Error if [str] is not of that form *)
let parse_params_full str =
  if Str.string_match (Str.regexp "size \\([0-9]\\)+") str 0
  then let size_str = Str.matched_string str in
    if Str.string_match (Str.regexp "attitude \\([0-9]\\)+")
      str (String.length size_str + 1)
    then let att_str = Str.matched_string str in
      if Str.string_match (Str.regexp "scarceness \\([0-9]\\)+") str
        (String.length size_str + String.length att_str + 2)
      then let scar_str = Str.matched_string str in
      let size_num = int_of_string (Str.string_after size_str 5) in
      let att_num_raw = int_of_string (Str.string_after att_str 9) in
      let att_num = if att_num_raw > 2 then 2 else att_num_raw in
      let scar_num = int_of_string (Str.string_after scar_str 11) in
      Params (size_num, att_num, scar_num)
      else raise Parsing_Error
    else raise Parsing_Error
  else raise Parsing_Error

(* [parse_params_full str] is the command that represents player input [str]
 * when starting the game, specifically when generating a new state using
 * size, attitude, and scarceness parameters.
 * requires: [str] is of the form "s _ a _ s _", where each
 * _ is a non-negative integer.
 * raises Parsing_Error if [str] is not of that form *)
let parse_params_short str =
  if Str.string_match (Str.regexp "s \\([0-9]\\)+") str 0
  then let size_str = Str.matched_string str in
    if Str.string_match (Str.regexp "a \\([0-9]\\)+")
      str (String.length size_str + 1)
    then let att_str = Str.matched_string str in
      if Str.string_match (Str.regexp "s \\([0-9]\\)+") str
        (String.length size_str + String.length att_str + 2)
      then let scar_str = Str.matched_string str in
      let size_num = int_of_string (Str.string_after size_str 2) in
      let att_num_raw = int_of_string (Str.string_after att_str 2) in
      let att_num = if att_num_raw > 2 then 2 else att_num_raw in
      let scar_num = int_of_string (Str.string_after scar_str 2) in
      Params (size_num, att_num, scar_num)
      else raise Parsing_Error
    else raise Parsing_Error
  else raise Parsing_Error

(* [parse_params_full str] is the command that represents player input [str]
 * when starting the game, specifically when generating a new state using
 * size, attitude, and scarceness parameters.
 * requires: [str] is of the form "_ _ _", where each
 * _ is a non-negative integer.
 * raises Parsing_Error if [str] is not of that form *)
let parse_params_nums str =
  if Str.string_match (Str.regexp "\\([0-9]\\)+") str 0
  then let size_str = Str.matched_string str in
    if Str.string_match (Str.regexp "\\([0-9]\\)+")
      str (String.length size_str + 1)
    then let att_str = Str.matched_string str in
      if Str.string_match (Str.regexp "\\([0-9]\\)+") str
        (String.length size_str + String.length att_str + 2)
      then let scar_str = Str.matched_string str in
      let size_num = int_of_string size_str in
      let att_num_raw = int_of_string att_str in
      let att_num = if att_num_raw > 2 then 2 else att_num_raw in
      let scar_num = int_of_string scar_str in
      Params (size_num, att_num, scar_num)
      else raise Parsing_Error
    else raise Parsing_Error
  else raise Parsing_Error

(* [parse_params_full str] is the command that represents player input [str]
 * when starting the game, specifically when loading a state file.
 * requires: [str] is of one of the following forms:
 * file _
 * filename _
 * _
 * where _ is the name of the file to be read*)
let parse_filename str =
  if Str.string_match (Str.regexp "file ") str 0
    then let filename = Str.string_after str 5 in Filename filename
  else if Str.string_match (Str.regexp "filename ") str 0
    then let filename = Str.string_after str 9 in Filename filename
  else let filename = str in Filename filename

let parse_start str =
  let str1 = String.lowercase_ascii str in
  if Str.string_match (Str.regexp
  "size \\([0-9]\\)+ attitude \\([0-9]\\)+ scarceness \\([0-9]\\)+")
  str1 0
    then let str_matched = Str.matched_string str1 in parse_params_full str_matched
  else if Str.string_match (Str.regexp
  "s \\([0-9]\\)+ a \\([0-9]\\)+ s \\([0-9]\\)+")
  str1 0
    then let str_matched = Str.matched_string str1 in parse_params_short str_matched
  else if Str.string_match (Str.regexp
  "\\([0-9]\\)+ \\([0-9]\\)+ \\([0-9]\\)+")
  str1 0
    then let str_matched = Str.matched_string str1 in parse_params_nums str_matched
  else parse_filename str1

let parse_game str =
  let str1 = String.lowercase_ascii str in
  if Str.string_match (Str.regexp "view all") str1 0
    then ViewAll
  else if Str.string_match (Str.regexp "status") str1 0
    then ViewAll
  else if Str.string_match (Str.regexp "save ") str1 0
    then let file_name = Str.string_after str 5 in Save file_name
  else if Str.string_match (Str.regexp "quit") str1 0
    then Quit
  else if Str.string_match (Str.regexp "step") str1 0
    then try
      let step_num = int_of_string (Str.string_after str1 5) in Step step_num
    with _ -> Step 1
  else if Str.string_match (Str.regexp "view ") str1 0
    then let tribe_name = Str.string_after str1 5 in View tribe_name
  else View str1
