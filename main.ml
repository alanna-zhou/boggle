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
  if State.end_state st 
  then (print_endline("Game Over \n Your score: " ^ string_of_int State.score);
        exit 0)
  else 
    print_string ("Words found: \n");
  print_string (make_list found_wrds "");
  print_endline "Enter a word";
  try  
    match Command.parse(read_line ()) with
    |Quit -> ()
    |Score -> print_string (string_of_int (State.score st))
    |Help -> 
    |Entry (guess) -> if List.mem guess found_wrds then
        print_string "This word has been guessed.";
      playing_game st found_wrds else
if Trie.contains st.words guess then
  playing_game (State.update st guess) (guess :: found_wrds) else
  print_string "This is not a valid word, choose again";
playing_game st found_wrds

let main () =
  ANSITerminal.(print_string [red]
                  "\n Welcome to Boggle.\n");
  print_endline "Press f to begin";
  print_string  "> "; 
  match read_line () with
  | "f"-> playing_game State.init []
  | _ -> ()
