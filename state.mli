type region = {
  name : string;
  area : int;
  climate : float;
  neighbors : (string * float) list;
}

type attd = Generous | Neutral | Agressive

type tribe = {
  name : string;
  pop : int;
  food : int;
  tools : int;
  weps : int;
  attd : attd;
  opins : (string * int) list;
  reg : string;
}
type state = {
  regions : (string * region) list;
  tribes : (string * tribe) list;
}

(* The string in Attack is the target *)
type action = Food | Tools | Weapons | Attack of string | Gift of (string * int)

(* [decide s t] is that action that tribe [t] will do, given the state [s] of 
 * the simulation. Contains helper functions that determine the benefit of the
 * different actions. The action the results is that which has the highest
 * benefit to the tribe *)
val decide : state -> tribe -> action

(* [do_action s t a] returns the new state after tribe [t] has done action
 * [a] on state [s] *)
val do_action : state -> tribe -> action -> state

(* [metabolize t] is the tribe [t] after the application of its metabolism
 * functions for one step. Metabolism functions affect the quantity of 
 * population and resources independently of any tribes' actions.
 * e.g. population will either grow or shrink depending on the available 
 * food. *)
val metabolize : tribe -> tribe

(* [step s i] is the state [s] after [i] steps. On each step, each tribe
 * performs one action and after every tribe acts, each tribe metabolizes *)
val step : state -> int -> state
