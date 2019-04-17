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

let board (state:t) : Board.t = 
  state.board

let words (state:t) : string list =
  Trie.to_list state.words

let update (state:t) (word:string) : t =
  let new_words = Trie.add_word state.words word in 
  let word_score = Board.word_score word state.board in 
  let new_score = state.score + word_score in 
  {board=state.board;score=new_score;words=new_words}
