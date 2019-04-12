
type node = {
  letter : char;
  position : int * int;
}
type size = int

type t = node list 

let generate (size:size) : t = 
  failwith "unimplemented"

let is_valid_word (word:string) (board:t) : bool = 
  failwith "unimplemented"

let word_score (word:string) (board:t) : int =
  failwith "unimplemented"

let format (formatter:Format.formatter) (board:t) : unit = 
  failwith "unimplemented"
