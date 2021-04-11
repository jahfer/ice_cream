open OUnit2

let suite = "Ice Cream" >::: [Ruby_type_test.suite; ]

let _ = run_test_tt_main suite