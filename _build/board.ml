open Trie 

type node = {
  letter : char;
  position : int;
}
type size = int

type t = {
  nodes : node list;
  words : Trie.t;
}

type board_type = Standard of size | Random of size 

let consonants = [|'B';'C';'D';'F';'G';'H';'J';'K';'L';'M';'N';'P';'Q';'R';'S';'T';'V';'W';'X';'Y';'Z'|]
let vowels = [|'A';'E';'I';'O';'U'|]

let english_words = Trie.empty 

(* Random.int 6 *)

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

(* let all_board_words (board:t) =  *)



let generate (board_type:board_type) : t = 
  match board_type with 
  | Standard size -> if size = 4 then generate_standard_4 else failwith "unimplemented"
  | Random size -> failwith "unimplemented"

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

let is_valid_word (word:string) (board:t) : bool = 
  failwith "unimplemented"
  (* let is_valid_acc = List.exists2 node_is_letter board [word.[0]] in 
  let rec check_neighbors (index:int) (word:string) (acc:bool) = begin
    if index = String.length word then acc
    else begin 
    let nodes = get_nodes (word.[index-1]) board in 
    let letter = word.[index] in 
    let is_valid = is_valid_neighbor node letter board in 
    (is_valid::acc)
    end 
  end 
  in check_neighbors 1 word is_valid_acc  *)


let word_score (word:string) (board:t) : int =
  String.length word 

let format (formatter:Format.formatter) (board:t) (size:int) : unit = 
  failwith "unimplemented"
