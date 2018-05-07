open OUnit2
open State

let r1 = {
  name = "a";
  area = 700;
  climate = 1.1;
  neighbors = [("b", 1.0)];
}

let r2 = {
  name = "b";
  area = 1000000;
  climate = 1.4;
  neighbors = [("a", 1.0)];
}

let t1 = {
  name = "a";
  pop = 1000;
  food = 1300;
  tools = 10;
  weps = 25;
  attd = Neutral;
  opins = [("b", 1)];
  reg = "a_reg";
}

let t2 = {
  name = "b";
  pop = 3000;
  food = 90;
  tools = 8;
  weps = 15;
  attd = Generous;
  opins = [("a", 3)];
  reg = "b_reg";
}

let s = {
  regions = [("a", r1); ("b", r2)];
  tribes = [("a", t1); ("b", t2)];
}

let t1_metabolize = {t1 with
pop = 1100;
food = 300;
}

let t2_metabolize = {t2 with
pop = 2030;
food = 0;
}

let t1_food = {t1 with
food = 700;
tools = 8;
}

let t2_food = {t2 with
food = 12793;
tools = 6;
}

let t1_tools = {t1 with
tools = 510
}

let t2_tools = {t2 with
tools = 1508
}

let t1_weapons = {t1 with
weps = 30;
tools = 7
}

let t2_weapons = {t2 with
weps = 19;
tools = 6
}

let tests =
[
  "metabolize 1" >:: (fun _ -> assert_equal t1_metabolize (metabolize t1));
  "metabolize 2" >:: (fun _ -> assert_equal t2_metabolize (metabolize t2));
  "food 1" >:: (fun _ -> assert_equal t1_food (List.assoc "a" (do_action s "a" Food).tribes));
  "food 2" >:: (fun _ -> assert_equal t2_food (List.assoc "b" (do_action s "b" Food).tribes));
  "tools 1" >:: (fun _ -> assert_equal t1_tools (List.assoc "a" (do_action s "a" Tools).tribes));
  "tools 2" >:: (fun _ -> assert_equal t2_tools (List.assoc "b" (do_action s "b" Tools).tribes));
  "weapons 1" >:: (fun _ -> assert_equal t1_weapons (List.assoc "a" (do_action s "a" Weapons).tribes));
  "weapons 2" >:: (fun _ -> assert_equal t2_weapons (List.assoc "b" (do_action s "b" Weapons).tribes));
]

let suite =
  "State test suite"
  >::: tests

let _ = run_test_tt_main suite
