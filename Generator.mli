
module Generator = sig 
	
	(* [generate_state size attitude scarceness] Creates a new world based off of the parameters
		given. [size] determines how many tribes there are, [attitude] is a measure of how 
		aggressive or generous tribes will be, and [scarceness] is a measure of how limited
		resources are in the world. 
	 *)
	val generate_state : int -> int -> int -> state 

	(* [save_state filename state] Saves [state] as a json object in [filename]. 
	 	- raises: FileError if saving failed. 
	 *)
	val save_state : string -> state -> unit

	(* [load_state filename] Tries to load in a state from file [filename]. 
		- raies: FileNotFound if file doesn't exist. 
	 *)
	val load_state : string -> state

end
