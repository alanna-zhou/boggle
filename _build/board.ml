
type node = {
  letter : char;
  position : int;
}
type size = int

type t = node list 

type board_type = Standard of size | Random of size 

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

let generate_standard_4 =
  let rec create_board (index:int) (board:t) = 
    if index < 0 then board else begin
      let die = Array.get standard_4 index in 
      let letter = random_char die 6 in 
      let node = create_node letter index in 
      create_board (index-1) (node::board)
    end 
  in create_board 15 []

let generate (board_type:board_type) : t = 
  match board_type with 
  | Standard size -> if size = 4 then generate_standard_4 else failwith "unimplemented"
  | Random size -> failwith "unimplemented"

let node_is_letter (node:node) (letter:char) : bool = 
  node.letter = letter

let positions_of_neighbors (node:node) (board:t) : int list =
  let size = int_of_float (Pervasives.sqrt ((float_of_int ((List.length board)+1)))) in 
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

let is_valid_neighbor (node:node) (letter:char) (board:t) : bool =
  failwith "unimplemented"

let is_valid_word (word:string) (board:t) : bool = 
  List.exists2 node_is_letter board [word.[0]]

let word_score (word:string) (board:t) : int =
  failwith "unimplemented"

let format (formatter:Format.formatter) (board:t) (size:int) : unit = 
  failwith "unimplemented"
