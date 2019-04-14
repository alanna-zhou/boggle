(* State represents the dynamic state of a game *)
open Board
open Trie

type t = {
  board : Board.t;
  score : int;
  words : Trie.t;
}

let init (board:Board.t) : t = 
  {board=board;score=0;words=Trie.empty}

let score (state:t) : int = 
  state.score

let update (state:t) (word:string) : t =
  failwith "unimplemented"