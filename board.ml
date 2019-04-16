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

let node0 = {letter='B';position=0}
let node1 = {letter='A';position=1}
let node2 = {letter='D';position=2}
let node3 = {letter='E';position=3}
let node4 = {letter='E';position=4}
let node5 = {letter='F';position=5}
let node6 = {letter='G';position=6}
let node7 = {letter='S';position=7}
let node8 = {letter='E';position=8}


let b = {nodes=[node0;node1;node2;node3];words=Trie.empty}

let testing = {nodes=[node0;node1;node2;node3;node4;node5;node6;node7;node8];words=Trie.empty}

(*let english_words = add_words_from_file "english.txt"*)

let dummy_trie = Trie.empty
let english_words = Trie.add_words dummy_trie ["BAD";"BADE";"BA";"BAE";"B";"BEG";"BEE";"SEE";"FEED"]

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

let rec process_neighbors q (node:node) (board:t) (str:string) (words_acc:Trie.t) visited (neighbors_lst:int list) : Trie.t = 
  match neighbors_lst with 
  | [] -> process_queue q node board str words_acc visited
  | pos::t -> begin
    if visited.(pos) = false then begin 
      let neighbor = get_node pos board in 
      let dummy = Queue.add neighbor q in 
      let dummy1 = visited.(pos) <- true in 
      let new_str = str ^ (Char.escaped neighbor.letter) in 
      if 1 = 1 then 
      (* if Trie.contains english_words new_str then  *)
      let words_acc = Trie.add_word words_acc new_str in 
      (* process_neighbors q node board new_str words_acc visited t *)
      process_queue q node board new_str words_acc visited
      else 
      (* process_neighbors q node board new_str words_acc visited t  *)
      process_queue q node board new_str words_acc visited
        end
    else process_neighbors q node board str words_acc visited t 
  end 

and process_queue q (node:node) (board:t) (str:string) (words_acc:Trie.t) visited : Trie.t = 
    if Queue.is_empty q then words_acc else
    let u = Queue.take q in 
    let neighbor_positions = positions_of_neighbors u board in 
    process_neighbors q node board str words_acc visited neighbor_positions
 
(* returns a list of valid english words starting with the character in the node parameter *)
let rec process_node (node:node) (board:t) (str:string) (words_acc:Trie.t): Trie.t = 
  let visited = Array.make (List.length board.nodes) false in 
  let q = Queue.create () in 
  let dummy = visited.(node.position) <- true in 
  let dummy1 = Queue.add node q in 
  process_queue q node board str words_acc visited 
    
(* returns a new board with the words attribute populated *)
let rec populate_board_words (nodes:node list) (board:t) (word_list:Trie.t) : Trie.t = 
  match nodes with 
  | [] -> word_list
  | h::t -> populate_board_words t board (Trie.combine word_list (process_node h board "" Trie.empty))

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
      | h :: t -> node_loop t (acc || process_node h 0 board word []) in
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



