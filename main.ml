open Board
open Command
open State

(** type to represent the game *)
type game = {
  state : State.t;
}

(** [make_list] returns a list as a string separated by semi-colons. *)
let rec make_list lst acc =
  match lst with 
  |[]-> acc
  |h::t-> make_list t (h ^ "; " ^ acc)

(** [start_game] starts the game and allows for user input.  *)
let rec start_game game_number leaderboard () =
  print_endline "\nWhat kind of board would you like?";
  print_string  "\nType s for Standard and r for Random. : "; 
  if game_number = 0 then begin 
    match  read_line () with
    |"s" -> playing_game (Unix.time() +. 90.) 
              (State.init (Board.generate (Standard (4))) []) [] game_number
    |"r" -> playing_game (Unix.time() +. 90.) 
              (State.init (Board.generate (Random(4))) [])  [] game_number
    |_-> print_endline "\nInvalid entry"; start_game game_number leaderboard ()
  end else begin 
    match read_line () with 
    |"s" -> playing_game (Unix.time() +. 90.) 
              (State.init (Board.generate (Standard (4))) leaderboard) [] game_number
    |"r" -> playing_game (Unix.time() +. 90.) 
              (State.init (Board.generate (Random(4))) leaderboard) [] game_number
    |_-> print_endline "\nInvalid entry"; start_game game_number leaderboard ()
  end
and playing_game time st found_wrds game_number =
  if is_game_over time then 
    end_game game_number st 
  else begin 
    try 
      print_string "\n"; 
      Board.format (State.board st) (Board.size (State.board st));
      print_string ("\nWords found: " ^ 
                    (make_list found_wrds "") ^ "\nEnter a word: ");
      match (Command.parse(read_line ())) with
      (*|Quit -> print_string "hi"; end_game game_number st*)
      |Score -> print_string ("\nYour score: " ^ string_of_int (State.score st));
        playing_game time st found_wrds game_number
      |Quit -> end_game game_number st 
      |Help -> print_string "\nTo enter a word, enter that word.
To see your current score, enter #score.
To quit/restart game, enter #quit.
To see instructions, enter #help.";
        playing_game time st found_wrds game_number
      |Entry (guess) -> 
        if is_game_over time then end_game game_number st 
        else if List.mem guess found_wrds then playing_game time st found_wrds game_number
        else if not (Board.is_valid_word guess (State.board st)) 
        then raise (Failure guess)
        else begin
          let new_state = State.update st guess in 
          playing_game time (new_state) (guess :: found_wrds) game_number
        end
    with 
    |Failure x -> print_endline (x ^ " is not a valid input."); 
      playing_game time st found_wrds game_number
    |Empty -> print_string "\nEntry is empty, choose another word.";
      playing_game time st found_wrds game_number
  end 

(** [end_game] ends the game.  *)
and end_game game_number st =
  print_string("\nGame Over \nYour score: " ^ (string_of_int (State.score st)));
  let new_leaderboard = add_leaderboard (leaderboard st) ([score st]) (size (board st)) []  in
  let () = print_leaderboard new_leaderboard in 
  (prompt_end game_number (new_leaderboard) ())

(** [prompt_end] asks for user input on whether or not they'd like to continue playing.  *)
and prompt_end game_number leaderboard () =
  print_string "\nPlay again? y/n : ";
  match read_line () with 
  |"y" -> start_game (game_number + 1) (leaderboard) () 
  |"n" -> () 
  |_ -> print_string "Not a valid input."; prompt_end game_number leaderboard ()
and is_game_over time =
  Unix.time () >= time

(** [main] initializes and executes the game.  *)
let main () =
  print_string "Welcome to Word Blitz! At any time, type #help for the gameplay
   instructions. \n";
  start_game 0 [] ()

let () = main ()



