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

let rec prompt_board_size () =
  try 
    let size = int_of_string (read_line()) in size
  with 
  |Failure x-> ANSITerminal.(print_string [red] "Invalid size, try again.");
    prompt_board_size ()

let rec prompt_board_file () =
  try 
    let file = read_line() in file
  with 
  |Failure x-> ANSITerminal.(print_string [red] "Invalid size, try again.");
    prompt_board_file ()

and prompt_board_type () =
  print_endline "\nWhat kind of board would you like?";
  print_string  "\nType s for Standard, r for Random, or c for Custom. "; 
  match  read_line () with
  |"s" -> print_string "Would you like the board to be a 4x4 or 5x5?";
    let s = prompt_board_size () in 
    if s =4 || s = 5 then
      playing_game (Unix.time() +. 90.) 
        (State.init (Board.generate (Standard (s)))) []
    else ANSITerminal.(print_string [red] "\nInvalid entry";
                       prompt_board_type ());

  |"r" -> print_string "What size board would you like? Entry must be less than 20.";
    let s = prompt_board_size () in 
    if s < 21
    then playing_game (Unix.time() +. 90.) 
        (State.init (Board.generate (Random (s)))) []
    else ANSITerminal.(print_string [red] 
                         ("\nRandom size must be less than or equal to 20.")); 
    prompt_board_type ()

  |"c" -> begin try 
        print_string "What size board would you like? Entry must be less than 20.";
        (let s = prompt_board_size () in 
         print_string "\nEnter the file name of your custom board";
         let f = prompt_board_file() in
         playing_game (Unix.time() +. 90.) 
           (State.init (Board.generate (Custom_board(f, s)))) [])
      with 
      |InvalidFile x -> ANSITerminal.(print_string [red] (x ^ " is not a valid file name"));
        prompt_board_type ()
    end

  |_-> ANSITerminal.(print_string [red] "\nInvalid entry"; prompt_board_type ());

and playing_game time st found_wrds =
  let () = Random.self_init () in 
  if is_game_over time
  then 
    end_game  st
  else  
    try 
      print_string "\n"; 
      Board.format (State.board st) (Board.size (State.board st));
      print_string ("\nWords found: " ^ 
                    (make_list found_wrds "") ^ "\nEnter a word: ");
      match Command.parse(read_line ()) with
      |Quit -> end_game st
      |Score -> print_string ("\nYour score: " ^ string_of_int (State.score st));
        playing_game time st found_wrds
      |Help -> print_string "\nTo guess a word, enter that word.
To see your current score, enter #score.
To quit/restart game, enter #quit.
To see instructions, enter #help.";
        playing_game time st found_wrds
      |Entry (guess) -> 
        ignore(clear 0);
        if is_game_over time then end_game st 
        else if List.mem guess found_wrds then playing_game time st found_wrds
        else if not (Board.is_valid_word guess (State.board st)) 
        then raise (Failure guess)
        else playing_game time (State.update st guess) (guess :: found_wrds)
      |Hint -> if State.num_hints st < 1 then (
      print_string "You are out of hints.";
      playing_game time st found_wrds) else 
      let hint_state = State.hint st in 
      print_string ("Your hint is: " ^ (snd hint_state));
      playing_game time (fst hint_state) found_wrds
    with 
    |Failure x -> ignore(clear 0);
      ANSITerminal.(print_string [red] (x));
      print_string (" is not a valid input."); 
      playing_game time st found_wrds
    |Empty -> ignore(clear 0); 
      ANSITerminal.(print_string [red] "\nEntry is empty, choose another word.");
      playing_game time st found_wrds

and end_game st =
  ANSITerminal.(print_string [red] "\nGame Over"); 
  print_string ("\nYour score: ");
  ANSITerminal.(print_string [green](string_of_int (State.score st))); 
  prompt_end ()

and prompt_end () =
  print_string "\nPlay again? y/n : ";
  match read_line () with 
  |"y" -> prompt_board_type ()
  |"n" -> ()
  |_ -> ANSITerminal.(print_string [red]"Not a valid input."); prompt_end ()

and is_game_over time =
  Unix.time () >= time


let main () =
  ignore (clear 0);
  print_string "Welcome to Word Blitz! Form and enter words contained on the
board by connecting letters horizontally, vertically, or diagonally.
At any time, type #help for gameplay instructions. \n";
  prompt_board_type ()

let () = main()



