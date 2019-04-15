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
  (*if State.is_end st
    then print_endline("Game Over \n Your score: " ^ (string_of_int (State.score st))
                       "\n Play again? y/n");
    try 
    match read_line () with
    |"y" -> print_endline "What size board would you like?";
      print_string  "Side length> "; 
      let s = read_line () in  
      playing_game (State.init (Board.generate (Standard(int_of_string s)))) []
    |"n" -> ()
    |_ -> raise (Failure "int_of_string")
    with 
    |Failure x -> print_endline "This is not a valid input."; 
    (playing_game st found_wrds)
    else 
    print_string ("Words found: \n");
    print_string (make_list found_wrds "");
    print_endline "Enter a word";
    try  
    match Command.parse(read_line ()) with
    |Quit -> playing_game State.end_state found_wrds
    |Score -> print_string (string_of_int (State.score st));
    playing_game st found_wrds
    |Help -> print_string "To enter a word, enter that word.\n
                          To see your current score, enter #score.\n
                          To quit/restart game, enter #quit.\n
                          To see instructions, enter #help.";
    playing_game st found_wrds

    |Entry (guess) -> 
    if guess = "" then raise (Command.Empty "Enter a non-empty guess.")
    else if List.mem guess found_wrds 
    then print_endline "You already got points for this, try again.";
    (playing_game st found_wrds)
    else State.update st guess
    with 
    |Failure x -> print_endline "This is not a valid input."; 
    (playing_game st found_wrds)
    |Command.Empty message -> print_endline message (playing_game st found_wrds)
  *) failwith "unimplemented"

let rec main () =
  (*print_string "Welcome to Boggle.\n";
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
  *) failwith "unimplemented"

