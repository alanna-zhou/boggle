(* State represents the dynamic state of a game *)
open Board
open Trie

(** type to represent a state *)
type t = {
  board : Board.t;
  score : int;
  words : Trie.t;
}

(** [init] initializes a state given a board. *)
let init (board:Board.t) : t = 
  {board=board;score=0;words=Trie.empty}

(** [score] provides the score of the list of words that the player has been entering to play the board in the state.  *)
let score (state:t) : int = 
  state.score

(** [board] provides the board that the state has been keeping track for the player.  *)
let board (state:t) : Board.t = 
  state.board

(** [words] provides the list of words that the player has been entering to play the game.  *)
let words (state:t) : string list =
  Trie.to_list state.words

(** [update] updates a state with a newly added word by validating if the word is a valid word (on the board and in the English language - this is dependent upon Board's implementation of [is_valid_word]). *)
let update (state:t) (word:string) : t =
  let new_words = Trie.add_word state.words word in 
  let word_score = Board.word_score word state.board in 
  let new_score = state.score + word_score in 
  {board=state.board;score=new_score;words=new_words}
