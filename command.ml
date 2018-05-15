type command =
 | Step of int
 | ViewAll
 | View of string
 | Save of string
 | Quit

type start =
 | Filename of string
 | Params of int * int * int

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
      let att_num = int_of_string (Str.string_after att_str 9) in
      let scar_num = int_of_string (Str.string_after scar_str 11) in
      Params (size_num, att_num, scar_num)
      else failwith "Parsing error"
    else failwith "Parsing error"
  else failwith "Parsing error"

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
      let att_num = int_of_string (Str.string_after att_str 2) in
      let scar_num = int_of_string (Str.string_after scar_str 2) in
      Params (size_num, att_num, scar_num)
      else failwith "Parsing error"
    else failwith "Parsing error"
  else failwith "Parsing error"

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
      let att_num = int_of_string att_str in
      let scar_num = int_of_string scar_str in
      Params (size_num, att_num, scar_num)
      else failwith "Parsing error"
    else failwith "Parsing error"
  else failwith "Parsing error"

let parse_filename str =
  if Str.string_match (Str.regexp "file ") str 0
    then let filename = Str.string_after str 5 in Filename filename
  else if Str.string_match (Str.regexp "filename ") str 0
    then let filename = Str.string_after str 9 in Filename filename
  else let filename = str in Filename filename

 (* options for params:
  * size _ attitude _ scarceness _
  * s _ a _ s _
  * _ _ _*)
 (* options for filename:
  * file _
  * filename _
  * _*)
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

let parse_game str = failwith "Undefined"
