type region = {
  name : string;
  area : int;
  climate : float;
  neighbors : (string * float) list;
  polygon : (int*int) array;
  base_color : int
}

type attd = Generous | Neutral | Aggressive

(* The string in Attack is the target *)
type action = Food | Tools | Weapons | Attack of string | Gift of (string * int)

type tribe = {
  name : string;
  pop : int;
  food : int;
  tools : int;
  weps : int;
  attd : attd;
  opins : (string * int) list;
  reg : string;
  last_action : action;
}
type state = {
  regions : (string * region) list;
  tribes : (string * tribe) list;
  turns : int;
}

(* [decide s name] is the [action] that tribe with name [name] will do, given
 * the state [s] of the simulation. The action the results is that which has 
 * the highest "desireability" to the tribe
 * - raises not_found if [name] is not associated with both a tribe and region 
 *   in s *)
val decide : state -> string -> action

(* [do_action s name a] returns the new state after tribe with name [name] has 
 * done action [a] on state [s]
 * - raises not_found if [name] is not associated with both a tribe and region
 *   in s *)
val do_action : state -> string -> action -> state

(* [metabolize t] is the tribe [t] after the application of its metabolism
 * functions for one step. Metabolism functions affect the quantity of
 * population and resources independently of any tribes' actions.
 * e.g. population will either grow or shrink depending on the available
 * food. *)
val metabolize : tribe -> tribe

(* [step s i] is the state [s] after [i] steps. On each step, each tribe
 * performs one action and after every tribe acts, each tribe metabolizes *)
val step : state -> int -> state
