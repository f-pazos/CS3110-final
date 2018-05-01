open List

(* - higher climate = more fertile land 
 * - area in [500..1000] 
 * - climate in [0.8..1.8] 
 * - float in neighbors related to the intensity of interactions, [0.8..1.8]
 *)
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

(* [min_opin o] is the lowest int opinion of the opins list [o] or the base 
 * int [i] *)
let rec min_opin o i = 
  match o with
  | [] -> i
  | (tr,op)::t -> if op < i then min_opin t op else min_opin t i

(* [max_opin o] is the highest int opinion of the opins list [o] or the base 
 * int [i] *)
let rec max_opin o i = 
  match o with
  | [] -> i
  | (tr,op)::t -> if op > i then max_opin t op else max_opin t i

(* [max l] is the maximum int of list [l] and base [i] *)
let rec max l:(int list) i = 
  match l with
  | [] -> i
  | h::t -> if h > i then max l h else max l i

(* [most_hated o] returns the string name associated with the lowest int 
 * in [o] *)
let most_hated o =
  fst (find (fun x -> snd x = (min_opin o 100)))

(* [most_liked o] returns the string name associated with the highest int 
 * in [o] *)
let most_liked o =
  fst (find (fun x -> snd x = (max_opin o -100)))

(* [decide s t] is the [action] that tribe [t] will do, given the state [s] of 
 * the simulation. The action the results is that which has the highest
 * "desireability" to the tribe *)
let decide s t =
  let food_des =
    (t.pop/t.food) * (if t.food < t.pop then 2 else 1) * climate
  in
  let tools_des =
    if t.tools > t.pop then 0
    else t.pop/t.tools
  in
  let weps_des =
    if t.weps > t.pop then 0
    else ((t.pop/t.weps)/2) * (if t.attd = Agressive then 2 else 1)
  in
  let attack_des =
    let lowest = min_opin t.opins 100 in
    if lowest > 0 then 0 else
      (abs (lowest)) * (if t.attd = Agressive then 2 else 1)
  in
  let gift_des =
    let highest = max_opin t.opins -100 in
    if highest < 0 then 0 else
      highest * (if t.attd = Generous then 2 else 1)
  in 
  let most = max (food_des::tools_des::weps_des::attack_des::[]) gift_des in
  if most = food_des then
    Food
  else if most = tools_des then
    Tools
  else if most = weps_des then
    Weapons
  else if most = attack_des then
    Attack(most_hated t.opins)
  else
    Gift((most_liked t.opins),50) (* 50 is a placeholder here *)

(* [do_action s t a] returns the [state] after tribe [t] has done action
 * [a] on state [s]
 * For a = :
 * - Food: Generates 3 food for every tribe member + 6 for each member with a
 * tool, multiplied by the climate. 1 out of every 4 tools used breaks. The
 * maximum food output is equal to the area.
 * - Tools: Generates 1 tool for every 2 tribe members
 * - Weapons: Generates 1 weapon for every 2 tribe members with tools. 1 out 
 * of every 3 tools used breaks.
 * - Attack: The attack success is a function of the relative population plus
 * the armed population on either side multipled by a random float [0.7..2.0]
 * The number of casulaties on either side is proportional to the num
 * of weapons in use on the other side. The number of food stolen from the 
 * target is propotional to the pop of the attacking side. Both of these
 * results are affected by the attack success multiplier. A slight advantage is
 * given to the defender. The target's opinion of the attacker decreases by a
 * constant rate. Half of the weapons used degrade.
 * - Gift: The actor's food decreases by the quantity of the gift, the target's
 * food increases by the quantity of the gift. The target's opinion of the actor
 * increases by 1 for every item of food given, with a base of 1
 *)
let do_action s t a =
  let popwtools = if t.tools > t.pop then t.pop else t.tools in
  match a with
  | Food -> 
    let f = t.food + ((3 * t.pop) + (6 * popwtools)) * climate in
    let food' = min f t.area
    let tools' = t.tools - (popwtools/4)
    let t' = {t with food = food'; tools = tools'} in
    let tribes' = (t.name, t')::(remove_assoc t.name s.tribes) in
    {s with tribes = tribes'}
  | Tools -> 
    let t' = {t with tools = (t.tools + (t.pop/2))} in
    let tribes' = (t.name, t')::(remove_assoc t.name s.tribes) in
    {s with tribes = tribes'}
  | Weapons -> 
    let weps' = t.weps + (popwtools/2) in
    let tools' = t.tools - (popwtools/3) in
    let t' = {t with weps = weps'; tools = tools'} in
    let tribes' = (t.name, t')::(remove_assoc t.name s.tribes) in
    {s with tribes = tribes'}
  | Attack(name) -> 
    let x = mem_assoc name s.tribes in
    let t_popwithweps = min t.pop t.weps in
    let x_popwithweps = min x.pop x.weps in
    let tforce = t.pop + t_popwithweps in
    let xforce = x.pop + x_popwithweps in
    let t_success = 
      ((t.pop + t_popwithweps)/(x.pop + x_popwithweps)) * 
        (((Random.int 80) + 70)/150.) - 0.15 in
    let x_success = (1/t_success) in
    let xpop' = x.pop - tforce * t_success in
    let tpop' = t.pop - xforce * x_success in
    let food_stolen = min x.food (t.pop * t_success) in
    let tfood' = t.food + food_stolen in
    let xfood' = x.food - food_stolen in
    let xopins' = 
      (t.name,((mem_assoc t.name x.opins) - 5))::(remove_assoc t.name x.opins)
    in
    let xweps' = x.weps/2 in
    let tweps' = t.weps/2 in
    let x' = 
      {x with pop = xpop'; food = xfood'; opins = xopins; weps = xweps'} in
    let t' = {t with pop = tpop'; food = tfood'; weps = tweps'} in
    let tribes' = (t.name, t')::(remove_assoc t.name s.tribes) in
    let tribes'' = (x.name, x')::(remove_assoc x.name tribes') in
    {s with tribes = tribes''}
  | Gift(g) -> 
    match g with =
    | (x,i) -> 
      let x = mem_assoc s.tribes in
      let newopin =
        (t.name,((mem_assoc t.name x.opins) + (max 1 i/5))) in
      let xopins' = newopin::(remove_assoc t.name x.opins) in
      let x' = {x with food = (x.food + i); opins = xopins'} in
      let t' = {t with food = (t.food - i)} in
      let tribes' = (t.name, t')::(remove_assoc t.name s.tribes) in
      let tribes'' = (x.name, x')::(remove_assoc x.name tribes') in
      {s with tribes = tribes''}

(* [metabolize t] is the tribe [t] after the application of its metabolism
 * functions for one step. Metabolism functions affect the quantity of 
 * population and food independently of any tribes' actions. Thus,
 * metabolize will be applied after the action function.
 * Notes: 
 * - each tribe member consumes 1 food per turn 
 * - food cannot be negative 
 * - pop increases by 1 for every 3 foods over the necessary 
 * - pop decreases by 1 for every 3 foods under the necessary
 * #EXTRA: This would be the function where we could add random events like 
 * Storm or disease
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