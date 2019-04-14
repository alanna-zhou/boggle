open OUnit2
open Board
open State

let suite = []

let board = Board.generate_standard_4

let _ = run_test_tt_main suite