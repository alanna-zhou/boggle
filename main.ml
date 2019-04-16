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

let rec playing_game time st found_wrds =
  if time<=Unix.time ()
  then 
    try print_string("Game Over \n Your score: " ^ 
                     (string_of_int (State.score st)) ^ "\n Play again? y/n \n"); 
      match read_line () with
      |"y" -> start_game ()
      |"n" -> ()
      |_ -> raise (Failure "Invalid Input")
    with 
    |Failure x -> print_endline "This is not a valid input."; 
      (playing_game time st found_wrds)
  else  
    try print_string ("Words found: \n" ^ (make_list found_wrds "")^ "\n Enter a word");
      match Command.parse(read_line ()) with
      |Quit -> playing_game (Unix.time ()) st found_wrds
      |Score -> print_string ("Your score: " ^ string_of_int (State.score st));
        playing_game time st found_wrds
      |Help -> print_string "To enter a word, enter that word.\n
                          To see your current score, enter #score.\n
                          To quit/restart game, enter #quit.\n
                          To see instructions, enter #help.";
        playing_game time st found_wrds
      |Entry (guess) -> 
        if (List.mem guess found_wrds)
        then playing_game time st found_wrds
        else (playing_game time (State.update st guess) (guess :: found_wrds))
    with 
    |Failure x -> print_endline "This is not a valid input."; 
      playing_game time st found_wrds
    |Empty -> print_string "Entry is empty, choose another word.";
      playing_game time st found_wrds

and  start_game () =
  print_endline "What size board would you like to play with?";
  print_string  "Side length> "; 
  try
    print_endline "What kind of board would you like?";
    print_string  "Type s for Standard and r for Random> "; 
    match  read_line () with
    |"s" -> playing_game (Unix.time() +. 90.) (State.init (Board.generate (Standard (4)))) []
    |"r" -> playing_game (Unix.time() +. 90.) (State.init (Board.generate (Random(4)))) []
    |_-> print_endline "Invalid entry"; start_game ()
  with 
  |Failure x-> print_endline "Invalid side length." ; start_game ()



let main () =
  print_string "Welcome to Boggle.\n";
  start_game ()

let () = main()



