(** 
   Representation of static board data.

   This module represents the board used for the game. It handles generating
   a board, checking if a word is valid in a board, and computing scores.
*)

(** The abstract type of values representing boards. *)
type t

(** The size of the square board, the height and width. *)
type size = int

(** Raised when word cannot be read on a board.*)
exception InvalidWord of string 

(** The desired board configuration, or the method through which the board
    will be generated. The standard configuration uses prespecified die to generate
    each letter on the board. *)
(*type configuration = Standard*)

(** [generate size] generates a board of dimensions [size] x [size]. It uses
    the standard die to decide each letter on the board.   *)
val generate : size -> t

(** [is_valid_word w b] check whether word [w] can be formed on board [b] 
    looking only in the horizontal and vertical directions. The word can appear 
    backward on the board. Raises [InvalidWord] if [w] is not a valid word in [b].*)
val is_valid_word: string -> t -> bool

(** [word_score w b] is the score of the word w in board b. The score is
    equivalent to the word length. 
    Requires: word w must be a valid word in board b. *)
val word_score: string -> t -> int











