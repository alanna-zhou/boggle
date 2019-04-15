open OUnit2
open Board

let is_valid_word_tests = [

]

let word_score_tests  = [

]

let state_tests = [

]

let suite = "test suite for A6" >::: List.flatten [
    is_valid_word_tests; 
    word_score_tests;
    state_tests
  ]

let _ = run_test_tt_main suite