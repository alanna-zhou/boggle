open Trie 

type node = {
  letter : char;
  position : int;
}
type size = int

exception InvalidSize of size 

type t = {
  nodes : node list;
  words : Trie.t;
  (* for now, words is not implemented, we are going to implement it for the next sprint using a BFS (part of our excellent scope) *)
}

type board_type = Standard of size | Random of size 

let consonants = [|'B';'C';'D';'F';'G';'H';'J';'K';'L';'M';
                   'N';'P';'Q';'R';'S';'T';'V';'W';'X';'Y';'Z'|]
let vowels = [|'A';'E';'I';'O';'U'|]

let english_words = add_words_from_file "usa.txt"


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

(** [create_node l p] creates a node, to be placed in a board, with
    the field letter set as l and position set as p. *)
let create_node (letter:char) (position:int) : node = 
  {letter= letter; position=position}

(** [random_char] generates a random character from the English alphabet. A 4x4 standard die has bound of 6. *)
let random_char die_arr (bound:int) =
  let random_int = Random.int bound in 
  Array.get die_arr random_int

let size board = 
  int_of_float (sqrt (float_of_int (List.length (board.nodes))))

(** [generate_random size] generates a random board of dimensions size x size.*)
let generate_random (size:int) = 
  let rec create_board (index:int) (board:t) = 
    if index < 0 then board else begin
      let vowels_or_consonants = Random.int 3 in 
      if vowels_or_consonants = 0 then begin
        let letter = Array.get consonants (Random.int (Array.length consonants)) 
        in 
        let node = create_node letter index in 
        create_board (index-1) {nodes=(node::board.nodes);words=board.words}
      end
      else begin 
        let letter = Array.get vowels (Random.int (Array.length vowels)) in 
        let node = create_node letter index in 
        create_board (index-1) {nodes=(node::board.nodes);words=board.words}
      end 
    end
  in create_board ((size*size)-1) {nodes=[];words=Trie.empty}

(** [generate_standard_4] generates a 4x4 standard board using preconfigured 
    die.*)
let generate_standard_4 =
  let () = Random.self_init() in
  let rec create_board (index:int) (board:t) = 
    if index < 0 then board else begin
      let die = Array.get standard_4 index in 
      let letter = random_char die 6 in 
      let node = create_node letter index in 
      create_board (index-1) {nodes=(node::board.nodes);words=board.words}
    end 
  in create_board 15 {nodes=[];words=Trie.empty}

(** [positions of neighbors node b] returns the list of positions of 
    neighboring elements on the board. *)
let positions_of_neighbors (node:node) (board:t) : int list =
  let size = int_of_float 
      (Pervasives.sqrt ((float_of_int ((List.length board.nodes)+1)))) in 
  let pos = node.position in 
  if (pos mod size) = 0 then begin 
    List.filter (fun x -> x >= 0 && x < (size*size)) 
      [pos+size;pos-size;pos+1;pos+size+1;pos-size+1]
  end 
  else if (pos mod size) = (size-1) then begin
    List.filter (fun x -> x >= 0 && x < (size*size)) 
      [pos-size;pos-1;pos-size-1;pos+size-1;pos+size]
  end 
  else if (pos / size) = 0 then begin
    List.filter (fun x -> x >= 0 && x < (size*size)) 
      [pos+size;pos-1;pos+1;pos+size+1;pos+size-1] 
  end 
  else if (pos / size) = (size-1) then begin
    List.filter (fun x -> x >= 0 && x < (size*size)) 
      [pos-size;pos-size-1;pos+1;pos-1;pos-size+1] 
  end 
  else begin 
    List.filter (fun x -> x >= 0 && x < (size*size)) 
      [pos-size;pos-size-1;pos+size-1;pos+1;pos-1;pos+size+1;pos-size+1;
       pos+size] 
  end 

(** [get_node index board] returns the node with index [index] in [board]'s
    node list.*)
let get_node (index:int) (board:t) : node = 
  List.nth board.nodes index

(** [letters_of_neighbors pos_list b] returns the list of letters of all nodes
    with the corresponding positions in pos_list *)
let letters_of_neighbors (pos_list:int list) (board:t) : char list =
  List.map (fun x -> let node = get_node x board in node.letter) pos_list

(** [generate] creates a board given a board type.  *)
let generate (board_type:board_type) : t = 
  match board_type with 
  | Standard size -> if size = 4 then generate_standard_4 
    else raise (InvalidSize size)
  | Random size -> (generate_random size)

(** [get_node_letter l lst acc] filters the node list [lst] and returns a
    only the nodes containing letter [l]. *)
let rec get_node_letter letter board_nodes acc =
  match board_nodes with
  | [] -> acc
  | h::t -> if (h.letter = letter) then (get_node_letter letter t (h::acc)) 
    else (get_node_letter letter t acc)

(** [validate_node node idx b str] does a depth first search for word [str] 
    in board [b], starting from node [node]. Returns true if [str] was found
    looking horizontally, vertically, and diagonally searching from the starting
    point, and returns false if not. *)
let rec validate_node (node:node) (index:int) (board:t) (str:string) 
    (visited_pos:int list) : bool = 
  let new_visited_pos = (node.position::visited_pos) in 
  if index = String.length str - 1 then true else begin
    let next_letter = str.[index + 1] in
    let neighbor_positions = positions_of_neighbors node board in 
    let possible_neighbors = List.filter 
        (fun x -> (get_node x board).letter = next_letter) neighbor_positions in
    let rec process_nlist neighbor_lst acc = match neighbor_lst with
      | [] -> acc
      | h::t -> begin
          if (List.mem h new_visited_pos = false) then begin
            process_nlist t (acc || (validate_node (get_node h board) 
                                       (index + 1) board str new_visited_pos))
          end else process_nlist t acc
        end in 
    (process_nlist possible_neighbors false)
  end

(** [is_valid_word w b] returns true if [w] is contained in the english
    dictionary and could be formed following the rules on board [b], and false
    otherwise. *)
let is_valid_word (word:string) (board:t) : bool = 
  if Trie.contains english_words (String.lowercase_ascii word) then begin 
    let upper_word = String.uppercase_ascii word in
    let first_char = upper_word.[0] in 
    let nodes_fst_letter = (get_node_letter first_char board.nodes []) in
    let rec node_loop lst acc = 
      match lst with
      | [] -> acc
      | h :: t -> node_loop t (acc || validate_node h 0 board upper_word []) in
    (node_loop nodes_fst_letter false)
  end else false

(** [word_score] computes the score of a word in the context of a board. *)
let word_score (word:string) (board:t) : int =
  String.length word 

(** [get_possible_words] gets all of the possible words of a board, which is contained in board.words (which we plan to populate via a future [populate_board] method)  *)
let get_possible_words (board:t) : string list = 
  Trie.to_list (board.words)

(** [format] formats the board in string form.  *)
let rec format board size = 
  match board.nodes with
  | [] -> ()
  | h::t -> begin
      let () = if (h.position + 1) mod size = 0 then begin
          (print_char h.letter ; print_string " " ; print_string "\n")
        end else 
          (print_char h.letter; print_string " ") in 
      (format {board with nodes=t} size)
    end 

(** Used to help test *)
let testing_board1 () = 
  let node_list = [{letter='I'; position=0}; 
                   {letter='F'; position=1}; 
                   {letter='D'; position=2}; 
                   {letter='D'; position=3}; 
                   {letter='M'; position=4}; 
                   {letter='S'; position=5}; 
                   {letter='A'; position=6}; 
                   {letter='Y'; position=7}; 
                   {letter='Q'; position=8}; 
                   {letter='P'; position=9}; 
                   {letter='T'; position=10}; 
                   {letter='R'; position=11}; 
                   {letter='N'; position=12}; 
                   {letter='A'; position=13};
                   {letter='I'; position=14}; 
                   {letter='C'; position=15};] in
  {nodes=node_list; words=Trie.empty}  




