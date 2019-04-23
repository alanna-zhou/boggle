open Board
open Command
open State

type game = {
  state : State.t;
}
let clear x = Sys.command("clear")+x
let rec make_list lst acc=
  match lst with 
  |[]-> acc
  |h::t-> make_list t (h ^ "; " ^ acc)

(** [start_game] starts the game and allows for user input.  *)
(*let rec start_game game_number leaderboard () =
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
    =======*)
let rec prompt_board_size () =
  try 
    let size = int_of_string (read_line()) in size
  with 
  |Failure x-> ANSITerminal.(print_string [red] "Invalid size, try again.");
    prompt_board_size ()

and prompt_board_type (game_number: int) (leaderboard: (int * int list) list) ()  =
  print_endline "\nWhat kind of board would you like?";
  print_string  "\nType s for Standard, r for Random, or c for Custom. "; 
  match  read_line () with
  |"s" -> print_string "Would you like the board to be a 4x4 or 5x5?";
    let s = prompt_board_size () in 
    if s =4 || s = 5 then
      playing_game (Unix.time() +. 90.) 
        (State.init (Board.generate (Standard (s))) 
           (if game_number = 0 then [] else leaderboard)) []
        game_number
    else ANSITerminal.(print_string [red] "\nInvalid entry";
                       prompt_board_type game_number leaderboard ());

  |"r" -> print_string "What size board would you like? 
      For example, entering 10 will create a 10x10 board. 
      Entry must be less than 30.";
    let s = prompt_board_size () in 
    if s < 31 && s > 5
    then playing_game (Unix.time() +. 90.) 
        (State.init (Board.generate (Random(s))) 
           (if game_number = 0 then [] else leaderboard)) []
        game_number
    else print_string "\nRandom size must be greater than or equal to 6, and 
        less than or equal to 30."; prompt_board_type game_number leaderboard ()

  |"c" ->     let s = prompt_board_size () in 
    playing_game (Unix.time() +. 90.) 
      (State.init (Board.generate (Random(s))) 
         (if game_number = 0 then [] else leaderboard)) []
      game_number
  |_-> ANSITerminal.(print_string [red] "\nInvalid entry"; 
                     prompt_board_type game_number leaderboard ());

and playing_game time (st: State.t) (found_wrds: string list) game_number =
  let () = Random.self_init () in 
  if is_game_over time
  then 
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
        ignore(clear 0);
        if is_game_over time then end_game game_number st 
        else if List.mem guess found_wrds then playing_game time st found_wrds 
            game_number
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
  ANSITerminal.(print_string [red] "\nGame Over)); 
  print_string (\nYour score: ");
  ANSITerminal.(print_string [green](string_of_int (State.score st))); 
  let new_leaderboard = add_leaderboard (leaderboard st) ([score st]) 
      (size (board st)) []  in
  let () = print_leaderboard new_leaderboard in 
  (prompt_end game_number (new_leaderboard) ())


(** [prompt_end] asks for user input on whether or not they'd like to continue playing.  *)
and prompt_end game_number leaderboard () =
  print_string "\nPlay again? y/n : ";
  match read_line () with 
  |"y" -> let () = prompt_board_type (game_number + 1) leaderboard () 
  |"n" -> () 
  |_ -> ANSITerminal.(print_string [red]"Not a valid input."); 
    prompt_end game_number leaderboard ()

and is_game_over time =
  Unix.time () >= time


let main () =
  ignore (clear 0);
  print_string "Welcome to Word Blitz! Form and enter words contained on the
board by connecting letters horizontally, vertically, or diagonally.
At any time, type #help for gameplay instructions. \n";
  prompt_board_type 0 [] ()

let () = main()

(*prompt_board_type should take game number and leaderboard*)


