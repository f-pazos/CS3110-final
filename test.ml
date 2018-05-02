open OUnit2
(* open State *)

let tests =
[
  "sample" >:: (fun _ -> assert_equal 1 1);
  "sample2" >:: (fun _ -> assert_equal 1 1);
]

let suite =
  "State test suite"
  >::: tests

let _ = run_test_tt_main suite
