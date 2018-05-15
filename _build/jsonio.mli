open State
(* [save_state filename state] Saves [state] as a json object in [filename].
   - raises: FileError if saving failed.
*)
val write_state : string -> state -> unit

(* [load_state filename] Tries to load in a state from file [filename].
   - raises: FileNotFound if file doesn't exist,
     FileError if there is a problem with reading the file
*)
val read_state : string -> state
