open OUnit2
open Board


let rec print_list = function 
    [] -> ()
  | e::l -> print_string e ; print_string " " ; print_list l


let suite = "test suite for A6" >::: List.flatten [

  ]

let board = generate (Standard 4) 
let () = format board 4
let () = print_list (get_possible_words board)


let _ = run_test_tt_main suite