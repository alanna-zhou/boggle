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
}

type board_type = Standard of size | Random of size 

let consonants = [|'B';'C';'D';'F';'G';'H';'J';'K';'L';'M';
                   'N';'P';'Q';'R';'S';'T';'V';'W';'X';'Y';'Z'|]
let vowels = [|'A';'E';'I';'O';'U'|]

let english_words = add_words_from_file "english.txt"

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

(* a 4x4 standard die has bound of 6 *)
let random_char die_arr (bound:int) =
  let random_int = Random.int bound in 
  Array.get die_arr random_int

let size board = 
  int_of_float (sqrt (float_of_int (List.length (board.nodes))))

(** [generate_random size] generates a random board of dimensions size x size.*)
let generate_random (size:int) = 
  let rec create_board (index:int) (board:t) = 
    if index < 0 then board else begin
      let zero_or_one = Random.int 2 in 
      if zero_or_one = 0 then begin
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
  let rec create_board (index:int) (board:t) = 
    if index < 0 then board else begin
      let die = Array.get standard_4 index in 
      let letter = random_char die 6 in 
      let node = create_node letter index in 
      create_board (index-1) {nodes=(node::board.nodes);words=board.words}
    end 
  in create_board 15 {nodes=[];words=Trie.empty}

(**[positions of neighbors node b] returns the list of positions of 
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

(* Gets the neighboring nodes *)
let nodes_of_neighbors (node:node) (board:t) : node list =
  let neighbor_positions = positions_of_neighbors node board in 
  let rec rec_nodes positions acc =
    match positions with 
    | [] -> acc 
    | h::t -> rec_nodes t ((get_node h board)::acc)
  in rec_nodes neighbor_positions []

(* Given a list of nodes, this will return the string formed by the nodes (but in reverse order) *)
let get_string_from_nodes (nodes:node list) : string = 
  let char_array = List.map (fun x -> x.letter) nodes in 
  let string_array = List.map (fun x -> Char.escaped x) char_array in 
  List.fold_left (fun acc x -> x^acc) "" string_array

(* sequence of nodes; gets neighbors of last node in the sequence, and then subtracts from these neighbors any nodes that are already present in the sequence *)
let get_BFS_neighbors (nodes:node list) board : node list =
  match nodes with
  | [] -> []
  | last_node::t -> let neighbor_nodes = nodes_of_neighbors last_node board in
    List.filter (fun x -> not (List.mem x nodes)) neighbor_nodes 

(* only adds one letter to the node_lst, for example, if the queue passed in "ABC" then this would only process one valid neighboring character, like "ABCD," "ABCE", "ABCF" ... and so on. *)
let rec process_neighbors q (node_lst:node list) (str:string) (board:t) (words_acc:Trie.t) (neighbor_nodes:node list) : Trie.t = 
  match neighbor_nodes with 
  | [] -> process_queue q board words_acc 
  | n_node::t -> begin 
    let new_nodes = n_node::node_lst in 
    let new_str = str^Char.escaped n_node.letter in 
    let words_acc = Trie.add_word words_acc new_str in 
    let dummy = Queue.add new_nodes q in 
    process_neighbors q node_lst str board words_acc t
  end 

(* processes the entire queue holding all of the possible node sequences; essentially a loop for while the queue isn't empty *)
and process_queue q (board:t) (words_acc:Trie.t) : Trie.t = 
    if Queue.is_empty q then words_acc else
    let node_lst = Queue.take q in 
    let neighbor_nodes = get_BFS_neighbors node_lst board in 
    let new_str = get_string_from_nodes node_lst in 
    let words_acc = Trie.add_word words_acc new_str in 
    process_neighbors q node_lst new_str board words_acc neighbor_nodes
 
(* returns a list of valid english words starting with the character in the node parameter *)
let rec process_node (nodes:(node list) list) (board:t) (words_acc:Trie.t): Trie.t = 
  let q = Queue.create () in 
  let dummy = List.map (fun x -> Queue.add x q) nodes in 
  (* let dummy = Queue.add nodes q in  *)
  process_queue q board words_acc  
    
(* helper function for [populate_board] to start the BFS algorithm on the ndoes of the board *)
let rec populate_board_words (nodes:node list) (board:t) (word_list:Trie.t) : Trie.t = 
  let nodes_for_q = List.fold_left (fun acc x -> [x]::acc) [] nodes in 
  process_node nodes_for_q board Trie.empty

(* actually populates the board with all the possible words that it can form *)
let rec populate_board (board:t) : t =
  let trie = populate_board_words board.nodes board Trie.empty in 
  (*let trie = Trie.add_words board.words found_words*) 
  {nodes=board.nodes;words=trie}

let generate (board_type:board_type) : t = 
  match board_type with 
  | Standard size -> if size =4 then generate_standard_4 
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
        (fun x -> (get_node x board).letter = next_letter) neighbor_positions
    in
    let rec process_neighbors neighbor_lst acc = match neighbor_lst with
      | [] -> acc
      | h::t -> begin
          if (List.mem h new_visited_pos = false) then begin
            process_neighbors t (validate_node (get_node h board) 
                                   (index + 1) board str new_visited_pos)
          end else process_neighbors t acc
        end in 
    (process_neighbors possible_neighbors true)
  end

(** [is_valid_word w b] returns true if [w] is contained in the english
    dictionary and could be formed following the rules on board [b], and false
    otherwise. *)
let is_valid_word (word:string) (board:t) : bool = 
  if Trie.contains english_words word then begin 
    let first_char = word.[0] in 
    let nodes_fst_letter = (get_node_letter first_char board.nodes []) in
    let rec node_loop lst acc = 
      match lst with
      | [] -> acc
      | h :: t -> node_loop t (acc || validate_node h 0 board word []) in
    (node_loop nodes_fst_letter false)
  end else false


let word_score (word:string) (board:t) : int =
  String.length word 

let get_possible_words (board:t) : string list = 
  Trie.to_list (board.words)

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



