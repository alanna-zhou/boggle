open Board
open Command
open State

type game = {
  state : State.t;
}
let rec make_list lst acc=
  match lst with 
  |[]-> acc
  |h::t-> make_list t (h ^ "; " ^ acc)

let rec playing_game st found_wrds =
  if State.is_end st
  then 
    try print_string("Game Over \n Your score: " ^ 
                     (string_of_int (State.score st)) ^ "\n Play again? y/n \n"); 
      match read_line () with
      |"y" -> print_string "What size board would you like? \n";
        print_string  "Side length> "; 
        playing_game (State.init (Board.generate (Standard(int_of_string (read_line ()))))) []
      |"n" -> ()
      |_ -> raise (Failure "Invalid Input")
    with 
    |Failure x -> print_endline "This is not a valid input."; 
      (playing_game st found_wrds)
  else  
    Board.format (State.board st) (Board.size State.board)
      try print_string ("Words found: \n" ^ (make_list found_wrds "")^ "\n Enter a word");
        match Command.parse(read_line ()) with
        |Empty -> print_string("Word is already guessed.") ;
          playing_game st found_wrds
        |Quit -> playing_game State.end_state found_wrds
        |Score -> print_string (string_of_int (State.score st));
          playing_game st found_wrds
        |Help -> print_string "To enter a word, enter that word.\n
                          To see your current score, enter #score.\n
                          To quit/restart game, enter #quit.\n
                          To see instructions, enter #help.";
          playing_game st found_wrds
        |Entry (guess) -> 
          if (List.mem guess found_wrds)
          then playing_game st found_wrds
          else (playing_game (State.update st guess) (guess :: found_wrds))
      with 
      |Failure x -> print_endline "This is not a valid input."; 
        (playing_game st found_wrds)

let rec main () =
  print_string "Welcome to Boggle.\n";
  print_endline "What size board would you like to play with?";
  print_string  "Side length> "; 
  try
    let side = int_of_string(read_line ()) in 
    print_endline "What kind of board would you like?";
    print_string  "Type s for Standard and r for Random> "; 
    match  read_line () with
    |"s" -> playing_game (State.init (Board.generate (Standard side))) []
    |"r" -> playing_game (State.init (Board.generate (Random side))) []
    |_-> print_endline "Invalid entry"; main ()
  with 
  |Failure x-> print_endline "Invalid side length." ; main ()

