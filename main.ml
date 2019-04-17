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

let rec start_game () =
  print_endline "\n What kind of board would you like?";
  print_string  "\n Type s for Standard and r for Random. :> "; 
  match  read_line () with
  |"s" -> playing_game (Unix.time() +. 90.) (State.init (let () = Random.self_init() in Board.generate (Standard (4)))) []
  |"r" -> playing_game (Unix.time() +. 90.) (State.init (Board.generate (Random(4)))) []
  |_-> print_endline "\n Invalid entry"; start_game ()

and playing_game time st found_wrds =
  let () = Random.self_init () in 
  if is_game_over time
  then 
    end_game  st
  else  
    try print_string "\n"; 
      Board.format (State.board st) (Board.size (State.board st));
      print_string ("\n Words found: " ^ (make_list found_wrds "") ^ "\n Enter a word:>");
      match Command.parse(read_line ()) with
      |Quit -> end_game st
      |Score -> print_string ("\n Your score: " ^ string_of_int (State.score st));
        playing_game time st found_wrds
      |Help -> print_string "\n To enter a word, enter that word.\n
                          To see your current score, enter #score.\n
                          To quit/restart game, enter #quit.\n
                          To see instructions, enter #help.";
        playing_game time st found_wrds
      |Entry (guess) -> 
        if is_game_over time then end_game st 
        else if List.mem guess found_wrds then playing_game time st found_wrds
        else if not (Board.is_valid_word guess (State.board st)) 
        then raise (Failure guess)
        else playing_game time (State.update st guess) (guess :: found_wrds)
    with 
    |Failure x -> print_endline (x ^ " is not a valid input."); 
      playing_game time st found_wrds
    |Empty -> print_string "\n Entry is empty, choose another word.";
      playing_game time st found_wrds

and end_game st =
  print_string("\n Game Over \n Your score: " ^ (string_of_int (State.score st))); 
  prompt_end ()

and prompt_end () =
  print_string "\n Play again? y/n :>";
  match read_line () with 
  |"y" -> start_game ()
  |"n" -> ()
  |_ -> print_string "Not a valid input."; prompt_end ()
and is_game_over time =
  Unix.time () >= time


let main () =
  print_string "Welcome to Word Blitz! At any time, type #help for the gameplay
   instructions. \n";
  start_game ()

let () = main()



