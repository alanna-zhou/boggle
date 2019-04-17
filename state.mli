(* State represents the dynamic state of a game *)
open Board

type t

val init : Board.t -> t

val board : t -> Board.t

val score : t -> int 

val words : t -> string list

val update : t -> string -> t
