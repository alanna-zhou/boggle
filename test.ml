open OUnit2
open Board
open State
open Trie

let board = testing_board1 ()
let board2 = testing_board2 ()
let board3 = testing_board3 ()

let state_0 = init board
let state_1 = update state_0 "i"
let state_2 = update state_1 "tip"
let state_3 = update state_2 "rat"

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2


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
]

let state_tests = [
  (**Checking if score is updated when word found on board.*)
  "update test 0" >:: (fun _ -> assert_equal 0 (score state_0));
  "update test 1" >:: (fun _ -> assert_equal 1 (score state_1));
  (* "update test 2" >:: (fun _ -> assert_equal 4 (score state_2)); *)
  (* "update test 3" >:: (fun _ -> assert_equal 7 (score state_3)); *)

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
  " B A
    D E board BFS" >:: (fun _ -> assert_equal (true)
                        (cmp_set_like_lists (get_possible_words board2) ["bade"; "bad"; "ba"; "bead"; "bea"; "beda"; "bed"; "be"; "b"; "abd"; "abed";
 "abe"; "ab"; "ade"; "ad"; "ae"; "a"; "dea"; "deba"; "deb"; "de"; "dab"; "dae";
 "da"; "d"; "ea"; "ed"; "eb"; "e"] ));
  " B A T
    D E L
    S N E board BFS" >:: (fun _ -> assert_equal (true)
                        (cmp_set_like_lists (get_possible_words board3) ["bates"; "bated"; "bate"; "bat"; "bales"; "bale"; "bal"; "bade"; "bad"; "ba";
 "beat"; "beads"; "bead"; "bea"; "bes"; "belt"; "belat"; "bela"; "bele"; "bel";
 "beds"; "beda"; "bed"; "beel"; "been"; "bee"; "beta"; "bet"; "bends"; "bend";
 "bene"; "ben"; "be"; "b"; "ae"; "abd"; "abel"; "abed"; "abet"; "abe"; "ab";
 "ate"; "at"; "alte"; "ales"; "ale"; "al"; "adel"; "aden"; "ade"; "ad"; "a";
 "tabe"; "tab"; "taleb"; "tales"; "talen"; "tale"; "tal"; "tad"; "ta"; "teal";
 "tea"; "tele"; "tel"; "ted"; "teens"; "teen"; "tee"; "tends"; "tend"; "tene";
 "tens"; "ten"; "te"; "t"; "dealt"; "deale"; "deal"; "dea"; "deba"; "deb";
 "des"; "delta"; "dela"; "dele"; "del"; "dee"; "det"; "dens"; "den"; "de";
 "dn"; "dae"; "dab"; "dates"; "datel"; "date"; "dat"; "dales"; "dale"; "dal";
 "da"; "d"; "labe"; "lab"; "lates"; "latene"; "laten"; "late"; "lat"; "lads";
 "laden"; "lade"; "lad"; "la"; "leads"; "lead"; "lea"; "leb"; "les"; "led";
 "let"; "lends"; "lende"; "lend"; "lens"; "len"; "lees"; "leeds"; "leed";
 "lee"; "le"; "l"; "sn"; "seab"; "seat"; "seale"; "seal"; "sea"; "seb"; "sele";
 "sel"; "sed"; "seen"; "see"; "set"; "send"; "sene"; "sen"; "se"; "s"; "neat";
 "nea"; "neb"; "nes"; "net"; "nel"; "needs"; "need"; "nee"; "ne"; "n"; "eat";
 "ea"; "eb"; "es"; "ed"; "eta"; "et"; "ele"; "elates"; "elated"; "elate";
 "elab"; "ela"; "el"; "ene"; "ends"; "endea"; "ende"; "enda"; "end"; "ens";
 "enl"; "en"; "eel"; "ee"; "e"]));
                        (* missing seat and dealt *)
]

let suite = "test suite for A6" >::: List.flatten [
    board_tests; 
    state_tests; 
    trie_tests;
    bfs_tests;
  ]

let _ = run_test_tt_main suite