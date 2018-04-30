open List

type region = {
	name : string;
	area : int;
	climate : int;
	neighbors : (string * int) list;
}

type tribe = {
	name : string;
	pop : int;
	food : int;
	tools : int;
	weapons : int;
	attitude : int;
	opinions : (string * int) list;
	regions : string list;
}
type state = {
	regions : region list;
	tribes : (string * tribe) list;
}

(* Has the name of the resource given, the quantity, and the target of the
 * gift. *)
type gift = (string * int * string)

(* The string in Attack is the target *)
type action = Food | Tools | Weapons | Attack of string | Gift of gift

(* [decide s t] is that action that tribe [t] will do, given the state [s] of 
 * the simulation. Contains helper functions that determine the benefit of the
 * different actions. The action the results is that which has the highest
 * benefit to the tribe *)
let decide s t =

(* [do_action s t a] returns the new state after tribe [t] has done action
 * [a] on state [s] *)
let do_action s t a =
	match a with
	| Food -> 
		let t' = {t with food = (t.food + (t.pop * 4))}
		let tribes' = (t.name, t')::(remove_assoc t.name s.tribes)
	| Tools -> 
	| Weapons -> 
	| Attack(s) -> 
	| Gift(g) -> 

(* [metabolize t] is the tribe [t] after the application of its metabolism
 * functions for one step. Metabolism functions affect the quantity of 
 * population and food independently of any tribes' actions. Thus,
 * metabolize will be applied after the action function.
 * Notes: 
 * - each tribe member consumes 1 food per turn 
 * - food cannot be negative 
 * - pop increases by 1 for every 3 foods over the necessary 
 * - pop decreases by 1 for every 3 foods under the necessary
 *)
let metabolize t:tribe =
	let food' =
		if t.food < t.pop then 0
		else t.food - t.pop
	in
	let pop' = 
		if t.food < t.pop then
			t.pop - ((t.food - t.pop) / 3)
		else if t.food = t.pop then t.pop
		else t.pop + ((t.food - t.pop) / 3)
	in
	{t with pop = pop'; food = food'}
