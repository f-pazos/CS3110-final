open OUnit2
open State

let r1 = {
  name = "a";
  area = 700;
  climate = 1.1;
  neighbors = [("b", 1.0);("c",1.0)];
  base_color = 100;
  polygon = [||]
}

let r2 = {
  name = "b";
  area = 1000000;
  climate = 1.4;
  neighbors = [("a", 1.0)];
  base_color = 100;
  polygon = [| |]
}


let r3 = {
  name = "c";
  area = 500;
  climate = 1.5;
  neighbors = [("a",1.0)];
  base_color = 100;
  polygon = [||]
}

let t1 = {
  name = "a";
  pop = 1000;
  food = 1300;
  tools = 10;
  weps = 25;
  attd = Neutral;
  opins = [("b", 1); ("c",2)];
  reg = "a_reg";
  last_action = Food;
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
  last_action = Food;
}

let t3 = {
  name = "c";
  pop = 5;
  food = 40;
  tools = 10;
  weps = 9;
  attd= Generous;
  opins = [("a",2)];
  reg = "c_reg";
  last_action = Food;
}
let t1postgift = { t1 with
                   food = 1299;
                   last_action= Gift ("b",1)

}
let t1postgift1 = {t1 with
                   food = 1298;
                   last_action= Gift ("c",2)
}
let t2postgift = {t2 with
  food = 91;
  opins = [("a", 4)];
}

let t3postgift3 = {t3 with
                   food=29;
                   last_action= Gift ("a",11)
}
let t1postgift3 = {t1 with
  food= 101;
  opins= [("c",3); ("b", 1)];

}

let t3postgift1 = {t3 with
  food= 42;
  opins = [("a",3)];

}
let t3_weapons = {t3 with
  weps=11;
  tools=9;
  last_action = Weapons;
}
let t3_tools = {t3 with
                tools=12;
                last_action= Tools;
}
let t3_food = {t3 with
  food= 127;
  tools=9;
}

let t3_metabolize = {t3 with
  pop = 16;
  food= 35;
}
let s = {
  regions = [("a", r1); ("b", r2);("c", r3)];
  tribes = [("a", t1); ("b", t2); ("c", t3)];
  turns = 0;
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
                tools = 510;
                last_action= Tools;
}

let t2_tools = {t2 with
                tools = 1508;
                last_action= Tools;
}

let t1_weapons = {t1 with
weps = 30;
tools = 7;
last_action= Weapons;
}

let t2_weapons = {t2 with
weps = 19;
tools = 6;
last_action= Weapons;
}

let tests =
[
  "metabolize 1" >:: (fun _ -> assert_equal t1_metabolize (metabolize t1));
  "metabolize 2" >:: (fun _ -> assert_equal t2_metabolize (metabolize t2));
  "metabolize 3" >:: (fun _ -> assert_equal t3_metabolize (metabolize t3));
  "food 1" >:: (fun _ -> assert_equal t1_food (List.assoc "a" (do_action s "a" Food).tribes));
  "food 2" >:: (fun _ -> assert_equal t2_food (List.assoc "b" (do_action s "b" Food).tribes));
  "food 3" >:: (fun _ -> assert_equal t3_food (List.assoc "c" (do_action s "c" Food).tribes));
  "tools 1" >:: (fun _ -> assert_equal t1_tools (List.assoc "a" (do_action s "a" Tools).tribes));
  "tools 2" >:: (fun _ -> assert_equal t2_tools (List.assoc "b" (do_action s "b" Tools).tribes));
  "tools 3" >:: (fun _ -> assert_equal t3_tools (List.assoc "c" (do_action s "c" Tools).tribes));
  "weapons 1" >:: (fun _ -> assert_equal t1_weapons (List.assoc "a" (do_action s "a" Weapons).tribes));
  "weapons 2" >:: (fun _ -> assert_equal t2_weapons (List.assoc "b" (do_action s "b" Weapons).tribes));
  "weapons 3" >:: (fun _ -> assert_equal t3_weapons (List.assoc "c" (do_action s "c" Weapons).tribes));
  "gift 1" >:: (fun _ -> assert_equal t1postgift (List.assoc "a" (do_action s "a" (Gift ("b", 1))).tribes));
  "gift 2" >:: (fun _ -> assert_equal t2postgift (List.assoc "b" (do_action s "a" (Gift ("b", 1))).tribes));
  "gift 3" >:: (fun _ -> assert_equal t1postgift1 (List.assoc "a" (do_action s "a" (Gift ("c", 2))).tribes));
  "gift 4" >:: (fun _ -> assert_equal t3postgift1 (List.assoc "c" (do_action s "a" (Gift ("c", 2))).tribes));
  "gift 5" >:: (fun _ -> assert_equal t3postgift3 (List.assoc "c" (do_action s "c" (Gift ("a", 11))).tribes));
  "attack1" >:: (fun _ -> assert_equal 1390 (List.assoc "a" (do_action s "a" (Attack "b")).tribes).food);
  "attack2" >:: (fun _ -> assert_equal 0 (List.assoc "b" (do_action s "a" (Attack "b")).tribes).food);
  "attack3" >:: (fun _ -> assert_equal 12 (List.assoc "a" (do_action s "a" (Attack "b")).tribes).weps);
  "attack4" >:: (fun _ -> assert_equal 7 (List.assoc "b" (do_action s "a" (Attack "b")).tribes).weps);
  "attack5" >:: (fun _ -> assert_equal [("a", -2)] (List.assoc "b" (do_action s "a" (Attack "b")).tribes).opins);
  "attack6" >:: (fun _ -> assert_equal 1300 (List.assoc "a" (do_action s "c" (Attack "a")).tribes).food);
  "attack7" >:: (fun _ -> assert_equal 40 (List.assoc "c" (do_action s "c" (Attack "a")).tribes).food);
  "attack8" >:: (fun _ -> assert_equal 12 (List.assoc "a" (do_action s "c" (Attack "a")).tribes).weps);
  "attack9" >:: (fun _ -> assert_equal 4 (List.assoc "c" (do_action s "c" (Attack "a")).tribes).weps);
  "attack10" >:: (fun _ -> assert_equal [("c", -3);("b",1)] (List.assoc "a" (do_action s "c" (Attack "a")).tribes).opins);
  "attack11" >:: (fun _ -> assert_equal 999 (List.assoc "a" (do_action s "c" (Attack "a")).tribes).pop);
  "attack12" >:: (fun _ -> assert_equal 0 (List.assoc "c" (do_action s "c" (Attack "a")).tribes).pop);
  "decide 1" >:: (fun _ -> assert_equal Tools (decide s "a"));
  "decide 2" >:: (fun _ -> assert_equal Tools (decide s "b"));
  "decide 3" >:: (fun _ -> assert_equal (Gift("a",11)) (decide s "c"));
  "step 0" >:: (fun _ -> assert_equal s (step s 0));
]

let suite =
  "State test suite"
  >::: tests

let _ = run_test_tt_main suite
