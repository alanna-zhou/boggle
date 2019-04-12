(* State represents the dynamic state of a game *)
open Board
open Trie

type t = {
  board : Board.t;
  score : int;
  words : Trie.t;
}

let init (board:Board.t) : t = 
  failwith "unimplemented"

let score (state:t) : int = 
  failwith "unimplemented"

let update (state:t) (word:string) : t =
  failwith "unimplemented"