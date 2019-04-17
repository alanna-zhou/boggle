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


let board = generate (Standard 4) 
let () = print_string (String.lowercase_ascii "RYE")
let x = is_valid_word ("TIE") board 
let () = if x = true then print_string "true \n" else print_string "false \n"
let () = format board 4

let _ = run_test_tt_main suite