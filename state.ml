(* State represents the dynamic state of a game *)
open Board
open Trie

(** type to represent a state *)
type t = {
  board : Board.t;
  score : int;
  words : Trie.t;
  leaderboard: (int * (int list)) list;
  hints_left : int;
  start_time : float;
  last_times : float list;
}

(** [init] initializes a state given a board. *)
let init (board:Board.t) (leaderboard): t = 
  {board=board;score=0;words=Trie.empty;leaderboard = leaderboard;
   hints_left=3;start_time=Unix.time ();
   last_times=[Unix.time ();Unix.time ();Unix.time ();Unix.time ();
               Unix.time ()]}


(** [score] provides the score of the list of words that the player has been 
    entering to play the board in the state.  *)
let score (state:t) : int = 
  state.score

(** [board] provides the board that the state has been keeping track for the 
    player.  *)
let board (state:t) : Board.t = 
  state.board

(** [words] provides the list of words that the player has been entering to 
    play the game.  *)
let words (state:t) : string list =
  Trie.to_list state.words


let leaderboard (state:t) = 
  state.leaderboard



let hint (state:t) : t =
  if state.hints_left <= 0 then failwith "No hints remaining" else
    {board = state.board; score = state.score - 5; words = state.words;
     leaderboard = state.leaderboard; 
     hints_left = state.hints_left - 1;start_time = state.start_time;
     last_times = state.last_times}

let rec get_dif times =
  match times with
  | [] -> 1000.0
  | x::xs -> begin
      match xs with
      | [] -> 1000.0
      | y::ys -> x -. y
    end

let fast_enough times =
  match times with
  | [] -> false
  | x::xs -> let time_between = get_dif times in 
    time_between < 5.0

let multiplier score = 
  score * 3



(** [update] updates a state with a newly added word by validating if the word 
    is a valid word (on the board and in the English language - this is dependent 
    upon Board's implementation of [is_valid_word]). *)
let update (state:t) (word:string) : t =
  let times = (Unix.time ())::state.last_times in 
  let new_words = Trie.add_word state.words word in 
  let word_score = Board.word_score word state.board in 
  let new_score = if fast_enough times 
    then state.score + (multiplier word_score) else state.score + word_score in
  {board=state.board;score=new_score;words=new_words; 
   leaderboard = (leaderboard state);
   hints_left=state.hints_left;start_time=state.start_time;
   last_times=(Unix.time ())::(state.last_times)}

(**[key_compare x y] is a comparison function to sort the leaderboard.*)
let key_compare x y = 
  if fst x < fst y then -1
  else if fst x > fst y then 1
  else 0 

(**[print_score_list lst] is a helper function to print the
   score list [lst] for each size within a leaderboard. *)
let rec print_score_list = function
  | [] -> ()
  | h::t -> print_int h; print_string "\n"; print_score_list t

(**[print_leaderboard_helper l] is a helper function to print the
   leaderboard [l]. *)
let rec print_leaderboard_helper l = 
  match l with
  | [] -> ()
  | (size, score_lst)::t -> let str_size = string_of_int size in 
    let size_string = "\n"^str_size^"x"^str_size^":\n" in 
    ANSITerminal.(print_string [green] size_string); 
    print_score_list score_lst; 
    print_leaderboard_helper t 

let print_leaderboard l = print_string "\n"; 
  if l = [] then () else 
    (ANSITerminal.(print_string [red] "******LEADERBOARD********"); 
     print_leaderboard_helper (List.rev (List.sort key_compare l)))

let rec add_leaderboard (old_leaderboard) (score:int list) (size:int) (acc) =  
  match old_leaderboard with 
  | [] -> ((size, score)::acc)
  | (h_size, lst)::t -> if h_size = size then begin
      let new_lst = (score@lst) in 
      let sorted_list = List.rev (List.sort compare new_lst) in 
      add_leaderboard t sorted_list size (acc) 
    end else add_leaderboard t score size ((h_size, lst)::acc)

