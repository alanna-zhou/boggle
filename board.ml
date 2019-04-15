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

let consonants = [|'B';'C';'D';'F';'G';'H';'J';'K';'L';'M';'N';'P';'Q';'R';'S';'T';'V';'W';'X';'Y';'Z'|]
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

let testing = {nodes=[node0;node1;node2;node3;node4;node5;node6;node7;node8];words=Trie.empty}

(*let english_words = add_words_from_file "english.txt"*)

let dummy_trie = Trie.empty
let english_words = Trie.add_words dummy_trie ["BAD";"BEG";"BEE";"SEE";"FEED"]

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

let standard_4 = [|die_0;die_1;die_2;die_3;die_4;die_5;die_6;die_7;die_8;die_9;die_10;die_11;die_12;die_13;die_14;die_15;|]

let create_node (letter:char) (position:int) : node = 
  {letter= letter; position=position}

(* a 4x4 standard die has bound of 6 *)
let random_char die_arr (bound:int) =
  let random_int = Random.int bound in 
  Array.get die_arr random_int

let generate_random (size:int) = 
  let rec create_board (index:int) (board:t) = 
    if index < 0 then board else begin
      let zero_or_one = Random.int 2 in 
      if zero_or_one = 0 then begin
        let letter = Array.get consonants (Random.int (Array.length consonants)) in 
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

let generate_standard_4 =
  let rec create_board (index:int) (board:t) = 
    if index < 0 then board else begin
      let die = Array.get standard_4 index in 
      let letter = random_char die 6 in 
      let node = create_node letter index in 
      create_board (index-1) {nodes=(node::board.nodes);words=board.words}
    end 
  in create_board 15 {nodes=[];words=Trie.empty}

let node_is_letter (node:node) (letter:char) : bool = 
  node.letter = letter

let positions_of_neighbors (node:node) (board:t) : int list =
  let size = int_of_float (Pervasives.sqrt ((float_of_int ((List.length board.nodes)+1)))) in 
  let pos = node.position in 
  if (pos mod size) = 0 then begin 
    List.filter (fun x -> x >= 0 && x < (size*size)) [pos+size;pos-size;pos+1;pos+size+1;pos-size+1]
  end 
  else if (pos mod size) = (size-1) then begin
    List.filter (fun x -> x >= 0 && x < (size*size)) [pos-size;pos-1;pos-size-1;pos+size-1;pos+size]
  end 
  else if (pos / size) = 0 then begin
    List.filter (fun x -> x >= 0 && x < (size*size)) [pos+size;pos-1;pos+1;pos+size+1;pos+size-1] 
  end 
  else if (pos / size) = (size-1) then begin
    List.filter (fun x -> x >= 0 && x < (size*size)) [pos-size;pos-size-1;pos+1;pos-1;pos-size+1] 
  end 
  else begin 
    List.filter (fun x -> x >= 0 && x < (size*size)) [pos-size;pos-size-1;pos+size-1;pos+1;pos-1;pos+size+1;pos-size+1;pos+size] 
  end 

let get_node (index:int) (board:t) : node = 
  List.nth board.nodes index

let get_nodes (letter:char) (board:t) : node = 
  failwith "unimplemented"
(* List.filter (fun node -> node.letter = letter) board  *)

let letters_of_neighbors (pos_list:int list) (board:t) : char list =
  List.map (fun x -> let node = get_node x board in node.letter) pos_list

let is_valid_neighbor (node:node) (letter:char) (board:t) : bool =
  let pos_list = positions_of_neighbors node board in 
  let neighbors = letters_of_neighbors pos_list board in 
  List.mem letter neighbors 

(* returns a list of valid english words starting with the character in the node parameter *)
let rec process_node (node:node) (board:t) (str:string) (visited_pos:int list) : string list = 
  let new_visited_pos = (node.position::visited_pos) in 
  let new_str = str ^ (Char.escaped node.letter) in 
  if Trie.contains english_words new_str then 
    let new_board = 
      {nodes=board.nodes;words=(Trie.add_word board.words new_str)} in 
    let neighbor_positions = positions_of_neighbors node board in 
    let rec process_neighbors neighbor_lst (acc: string list): string list = match neighbor_lst with
      | [] -> acc
      | h::t -> if (List.mem h new_visited_pos = false) then 
          (let neighbor_node = get_node h board in 
           let updated_words = process_node neighbor_node new_board new_str new_visited_pos in 
           process_neighbors t (updated_words @ acc)) 
        else acc
    in process_neighbors neighbor_positions (Trie.to_list new_board.words) 
  else 
    let new_board = board in
    let neighbor_positions = positions_of_neighbors node board in 
    let rec process_neighbors neighbor_lst acc = match neighbor_lst with
      | [] -> acc
      | h::t -> if (List.mem h new_visited_pos = false) then 
          (let neighbor_node = get_node h board in 
           let new_words = process_node neighbor_node new_board new_str new_visited_pos in 
           process_neighbors t (new_words @ acc)) 
        else acc
    in process_neighbors neighbor_positions (Trie.to_list board.words) (*Converting this to a list to be able to add words*)

(* returns a new board with the words attribute populated *)
let rec populate_board_words (nodes:node list) (board:t) (word_list:Trie.t) : Trie.t = 
  match nodes with 
  | [] -> word_list
  | h::t -> populate_board_words t board (Trie.add_words word_list (process_node h board "" []))

let rec populate_board (board:t) : t =
  let trie = populate_board_words board.nodes board Trie.empty in 
  (*let trie = Trie.add_words board.words found_words*) 
  {nodes=board.nodes;words=trie}

let generate (board_type:board_type) : t = 
  match board_type with 
  | Standard size -> if size = 4 then populate_board generate_standard_4 
    else raise (InvalidSize size)
  | Random size -> populate_board (generate_random size)

let is_valid_word (word:string) (board:t) : bool = 
  Trie.contains board.words word

let word_score (word:string) (board:t) : int =
  String.length word 

let get_possible_words (board:t) : string list = 
  Trie.to_list (board.words)


let rec format board size = 
  match board.nodes with
  | [] -> ()
  | h::t -> begin
      let () = if (h.position + 1) mod size = 0 then (print_char h.letter ; 
                                                      print_string " " ; 
                                                      print_string "\n") else 
          (print_char h.letter; print_string " ") in (format {board with nodes=t} size)
    end 


