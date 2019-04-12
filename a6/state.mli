(* State represents the dynamic state of a game *)
open Board

type t

val init : Board.t -> t

val score : t -> int 

val update : t -> string -> t
