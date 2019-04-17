open Trie
(** 
   Representation of static board data.

   This module represents the board used for the game. It handles generating
   a board, checking if a word is valid in a board, and computing scores.
*)

(** The abstract type of values representing boards. *)
type t

(** The size of the square board, the height and width. *)
type size = int

(** Raised when board size [size] is not allowed. *)
exception InvalidSize of size 

(** The type of a board, where standard is configured using 6 sided die for
    each board position, and where random randomnly choose a letter from the
    alphabet for each board element. *)
type board_type = Standard of size | Random of size 


(** [generate size] generates a board of dimensions [size] x [size]. It uses
    the standard die to decide each letter on the board. 
    @param size is the size of the board you want to generate
    @returns board is the board created
    Raises: InvalidSize if a board of the specified size cannot be generated.
    Requires: Standard board must be of size 4, random boards can be of any
    size. *)
val generate : board_type -> t


(** [size b] returns the height/width of the board. 
    @param b is a board.
    @returns the size of the board. 
    Raises: Does not raise any exceptions.
    Requires: The board must be square. *)
val size: t -> int 

(** [is_valid_word w b] check whether word [w] can be formed on board [b] 
    both using horizontal, vertical, and diagonal directions, and whether
    it is contained in the english dictionary.
    The word can appear backward on the board.
    @param w is the word that is being check as valid.
    @param b is the board in which the word is being found.
    @returns is a bool true/false indicating whether the word was found.
    Raises: Does not raise any exceptions*)
val is_valid_word: string -> t -> bool

(** [word_score w b] is the score of the word w in board b. The score is
    equivalent to the word length. 
    @param w is the word whose score is desired.
    @param b is the board in which you want to check its score.
    @returns an integer denoting the score of the word.
    Requires: word w must be a valid word in board b.
    Raises: No Exceptions *)
val word_score: string -> t -> int

(** [get_possible_words b] is a list of all the possible words that the board
    can form. 
    @param b is the board you want to form words from
    @returns a string list containing all possible words
    Raises: No Exceptions *)
val get_possible_words: t -> string list

(** [format b size] is a printing function suitable for use
     with the toplevel's [#install_printer] directive.
     It outputs a textual representation of a board
     on the given formatter. 
     @param b is the board you want to print out
     @param size is the height/width of the board you want to print out.
     @returns unit 
     Requires: Board b must be square.
     Raises: No Exceptions*)
val format : t -> size -> unit 

val testing_board1: unit -> t












