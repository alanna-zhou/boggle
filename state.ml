(* State represents the dynamic state of a game *)
open Board
open Trie

type t = {
  board : Board.t;
  score : int;
  words : Trie.t;
}

val init : Board.t -> t

val score : t -> int 

val update : t -> string -> t