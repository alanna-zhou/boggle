open OUnit2
open Board
open State
open Trie




let board = testing_board1 ()
let board2 = testing_board2 ()
let board3 = testing_board3 ()
let empty_board = generate (Random 0)


let state_0 = init board []
let state_1 = update state_0 "i"
let state_2 = update state_1 "tip"
let state_3 = update state_2 "rat"

let leaderboard_new = [(4, [10; 20]); (5, [8])]
let next_state = init board leaderboard_new 
let next_state_2 = update next_state "hi"
let next_leaderboard = add_leaderboard (leaderboard next_state_2) [50] 5 []



let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2


let die_0 = [|'R';'I';'F';'O';'B';'X'|]
let die_1 = [|'I';'F';'E';'H';'E';'Y'|]
let die_2 = [|'D';'E';'N';'O';'W';'S'|]
let die_3 = [|'U';'T';'O';'K';'N';'D'|]
let die_4 = [|'H';'M';'S';'R';'A';'O'|]
let die_5 = [|'L';'U';'P';'E';'T';'S'|]
let die_6 = [|'A';'C';'I';'T';'O';'A'|]
let die_7 = [|'Y';'L';'G';'K';'U';'E'|]
let die_8 = [|'Q';'B';'M';'J';'O';'A'|]
let die_9 = [|'E';'H';'I';'S';'P';'N'|]
let die_10 = [|'V';'E';'T';'I';'G';'N'|]
let die_11 = [|'R';'I';'F';'O';'B';'X'|]
let die_12 = [|'E';'Z';'A';'V';'N';'D'|]
let die_13 = [|'R';'A';'L';'E';'S';'C'|]
let die_14 = [|'U';'W';'I';'L';'R';'G'|]
let die_15 = [|'P';'A';'C';'E';'M';'D'|]

let standard_4 = [|die_0;die_1;die_2;die_3;die_4;die_5;die_6;die_7;die_8;
                   die_9;die_10;die_11;die_12;die_13;die_14;die_15;|]

let die2_0 = [|'R';'I';'F';'O';'B';'X'|]
let die2_1 = [|'I';'F';'E';'H';'E';'Y'|]
let die2_2 = [|'D';'E';'N';'O';'W';'S'|]
let die2_3 = [|'H';'M';'S';'R';'A';'O'|]

let standard_2 = [|die2_0; die2_1; die2_2; die2_3|]

let board_tests = [
  "board size" >:: (fun _ -> assert_equal 4 (size board));

  (**Checking whether words of length 1, 2, 3 can be found on the board. *)
  (**Trying words that belong both to the english dictionary, and words that do 
     not - these should not be valid.*)
  "is_valid_word test1" >:: (fun _ -> assert_equal true 
                                (is_valid_word "rat" board));
  "is_valid_word test2" >:: (fun _ -> assert_equal false 
                                (is_valid_word "x" board));
  "is_valid_word test3" >:: (fun _ -> assert_equal false 
                                (is_valid_word "dope" board));
  "is_valid_word test4" >:: (fun _ -> assert_equal true 
                                (is_valid_word "tip" board));
  "is_valid_word test5" >:: (fun _ -> assert_equal true 
                                (is_valid_word "i" board));
  "is_valid_word test6" >:: (fun _ -> assert_equal true 
                                (is_valid_word "it" board));
  "is_valid_word test7" >:: (fun _ -> assert_equal false
                                (is_valid_word "yard" board));
  "is_valid_word test8" >:: (fun _ -> assert_equal false 
                                (is_valid_word "jfkdsjflkjsd" board));
  "is_valid_word test9" >:: (fun _ -> assert_equal false
                                (is_valid_word "yam" board));
  "is_valid_word test10" >:: (fun _ -> assert_equal false
                                 (is_valid_word "tipd" board));

  (**Checking word scores*)
  "score test1" >:: (fun _ -> assert_equal 5
                        (word_score "tip" board));
  "score test2" >:: (fun _ -> assert_equal 3
                        (word_score "rat" board));
  "score test3" >:: (fun _ -> assert_equal 1
                        (word_score "i" board));

  (** Custom die board *)
  (*"custom 4x4 dice" >:: (fun _ -> assert_equal standard_4 
                            (create_die_arr "4x4.txt" 4)); 
    "custom 2x2 dice" >:: (fun _ -> assert_equal standard_2 
                            (create_die_arr "2x2.txt" 2))*)
]

let state_tests = [
  (**Checking if score is updated when word found on board.*)
  "update test 0" >:: (fun _ -> assert_equal 0 (score state_0));
  "update test 1" >:: (fun _ -> assert_equal 3 (score state_1));
  "update test 2" >:: (fun _ -> assert_equal 18 (score state_2));
  "update test 3" >:: (fun _ -> assert_equal 27 (score state_3));

  (**Checking if found words are saved in state *)
  ("words test" >:: (fun _ -> assert_equal (true)
                        (cmp_set_like_lists ["i"; "tip"; "rat"] (words state_3))))
]

let trie0 = Trie.empty
let trie1 = Trie.add_words_from_file "words.txt"
let trie2 = Trie.add_word (Trie.add_word Trie.empty "computer") "computers"
let trie_tests = [
  "contains 'rat'" >:: (fun _ -> assert_equal true (contains trie1 "rat"));
  "contains 'rats'" >:: (fun _ -> assert_equal true (contains trie1 "rats"));
  "contains 'I'" >:: (fun _ -> assert_equal true (contains trie1 "I"));
  "contains 'computer'" >:: (fun _ -> assert_equal true (contains trie2 "computer"));
  "does not contain 'afdsasi'" >:: (fun _ -> assert_equal false (contains trie1 "afdsasi"));
  "empty trie is empty" >:: (fun _ -> assert_equal false (contains trie0 "anything"));
  "to_list test" >:: (fun _ -> assert_equal true (cmp_set_like_lists ["computer"; "computers"] (to_list trie2)))
]


let bfs_tests = [
  "empty board BFS" >:: (fun _ -> assert_equal (true)
                           (cmp_set_like_lists (get_possible_words empty_board) 
                              [] ));
  " B A
    D E board BFS" >:: (fun _ -> assert_equal (true)
                           (cmp_set_like_lists (get_possible_words board2) 
                              ["deb"; "dab"; "bade"; "bad"; "ba"; "bead"; "bed"; "be"; "abed"; "abe"; "ad";"a"; "ed"] ));
  " B A T
    D E L
    S N E board BFS" >:: (fun _ -> assert_equal (true)
                             (cmp_set_like_lists (get_possible_words board3) 
                                ["delta"; "dealt"; "deal"; "deb"; "dens"; "den"; "dates"; "date"; "dab"; "dales"; "dale"; "dal"; "late"; "lab"; "lads"; "laden"; "lade"; "lad"; "leads"; "lead"; "lea"; "led"; "let"; "lends"; "lend"; "lens"; "len"; "leeds"; "lees"; "lee"; "bales"; "bale"; "bates"; "bated"; "bate"; "bat"; "bade"; "bad"; "ba"; "beat"; "beads"; "bead"; "belt"; "beds"; "bed"; "been"; "bee"; "beta"; "bet"; "bends"; "bend"; "ben"; "be"; "abel"; "abed"; "abet"; "abe"; "ate"; "at"; "ales"; "ale"; "al"; "aden"; "ad"; "a"; "tales"; "tale"; "tab"; "ta"; "teal"; "tea"; "tele"; "tel"; "ted"; "teens"; "teen"; "tee"; "tends"; "tend"; "tens"; "ten"; "te"; "elates"; "elated"; "elate"; "eat"; "eta"; "ene"; "ends"; "end"; "ens"; "eel"; "ed"; "send"; "seat"; "seal"; "sea"; "seen"; "see"; "set"; "se"; "neat"; "net"; "needs"; "need"; "nee"; "ne"]));
]

(* helper function to get the list of letters from a (letter,color) list *)
let get_letters (lst:(char*color) list) : char list = 
  List.map (fun x -> fst x) lst 

(* helper function to get the list of colors from a (letter,color) list *)
let get_colors (lst:(char*color) list) : color list = 
  List.map (fun x -> snd x) lst 

(* testing for an empty string so the board should be colored white *)
let empty_board3_result = nodes_and_colors "" board3

(* testing for a word that is on the board, but not an english word, so it should be colored red *)
let bds_board3_result = nodes_and_colors "bds" board3

(* testing for a word that is on the board and an english word, so it should be colored green *)
let dealt_board3_result = nodes_and_colors "dealt" board3

let nodes_and_colors_tests = [
  "nodes_and_colors empty board3 letters" >:: (fun _ -> assert_equal (true) ((get_letters empty_board3_result) = ['B'; 'A'; 'T'; 'D'; 'E'; 'L'; 'S'; 'N'; 'E']) );
  "nodes_and_colors empty board3 colors" >:: (fun _ -> assert_equal (true) ((get_colors empty_board3_result) = [White; White; White; White; White; White; White; White; White]) );
  "nodes_and_colors bds board3 letters" >:: (fun _ -> assert_equal (true) ((get_letters bds_board3_result) = ['B'; 'A'; 'T'; 'D'; 'E'; 'L'; 'S'; 'N'; 'E']) );
  "nodes_and_colors bds board3 colors" >:: (fun _ -> assert_equal (true) ((get_colors bds_board3_result) = [Red; White; White; Red; White; White; Red; White; White]) );
  "nodes_and_colors dealt board3 letters" >:: (fun _ -> assert_equal (true) ((get_letters dealt_board3_result) = ['B'; 'A'; 'T'; 'D'; 'E'; 'L'; 'S'; 'N'; 'E']) );
  "nodes_and_colors dealt board3 colors" >:: (fun _ -> assert_equal (true) ((get_colors dealt_board3_result) = [White; Green; Green; Green; Green; Green; White; White; White]) );
]

let suite = "test suite for A6" >::: List.flatten [
    board_tests; 
    state_tests; 
    trie_tests;
    bfs_tests;
    nodes_and_colors_tests;
  ]


(*let board4x4 = generate (Custom_die ("4x4.txt", 4))
  let () = format board4x4 4 *)

let board4x4 = generate (Standard 4)
let () = format board4x4 4 

let board5x5 = generate (Standard 5)
let () = format board5x5 5 

(*let board6x6 = generate (Custom_die ("6x6.txt", 6))
  let () = format board6x6 6

  let board20x20 = generate (Custom_die ("20x20.txt", 20))
  let () = format board20x20 20*)

let board_custom = generate (Custom_board ("board1.txt", 4))
let () = format board_custom 4

let board_custom2 = generate (Custom_board ("board2.txt", 5))
let () = format board_custom2 5

let _ = run_test_tt_main suite