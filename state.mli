(* State represents the dynamic state of a game *)
open Board

(* The type representation of a state of a game. *)
type t


(** [init] creates a State.t given a Board.t so that a state can keep track of the board that the user is playing, the player's score, and the list of words that the player has been entering.  
    @param board the grid of letters for Word Blitz
    @return state of type t
    Raises: N/A
    Requires: the board must be a valid board of type Board.t
    Example: Given a 2 x 2 board of A B C D, the state should keep track of the this exact board, with a score of 0 (since no game has started yet), and an empty word list.  *)
val init : Board.t -> (int * (int list)) list ->  t

(** [board] gives the board that the state is currently keeping track of.  
    @param state of type t
    @return board of type Board.t
    Raises: N/A
    Requires: the state must be a valid state of type State.t
    Example: Given a state with a 2 x 2 board of A B C D, the state should return this exact board. *)
val board : t -> Board.t

(** [score] gives the score of the current state, that is, it calculates the score of each word in its word list in the context of the board that it is keeping track of.
    @param state of type t
    @return score an integer value 
    Raises: N/A
    Requires: the state must be a valid state of type State.t
    Example: Given a state keeping track of a 2 x 2 board of A B C D, a score of 10, and a word list of ["ABC";"AB"], [score] should return 10. The calculation of the score depends on Board's implementation. *)
val score : t -> int 

(** [words] gives the list of words that the player has been entering and that the state has been keeping track of.
    @param state of type t
    @return words of type string list
    Raises: N/A
    Requires: the state must be a valid state of type State.t
    Example: Given a state keeping track of a 2 x 2 board of A B C D, a score of 10, and a word list of ["ABC";"AB"], [words] should return ["ABC";"AB"]. The internal representation of the word list depends on Board's implementation, but this will always return a string list. *)
val words : t -> string list


(** [update] allows the state to be updated when a new word has been entered by a player.
    @param old_state of type t
    @param word of type string 
    @return new_state of type t 
    Raises: N/A
    Requires: the state must be a valid state of type State.t. This does not require, however, that the word being updated is necessarily a valid word on the board or even a valid word of the English language--it will do the check itself according to [Board.is_valid_word] so it will make the precautions necessary and not irrationally add words that aren't valid.  
    Example: Given a state keeping track of a 2 x 2 board of A B C D, a score of 10, and a word list of ["ABC";"AB"] and a new word "A", [update] will either return the same state with an updated word list of ["ABC";"AB";"A"] (if "A" is indeed both a valid word on the board and in the English language) or just return the same state it was given. *)
val update : t -> string -> t

val leaderboard: t -> (int * (int list)) list

val add_leaderboard: (int * (int list)) list -> int list -> int -> (int * (int list)) list -> (int * (int list)) list

val print_leaderboard: (int * (int list)) list -> unit
