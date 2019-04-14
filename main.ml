open Board
open Command
open State

type game = {
  state : State.t;
}
let play_game () =
  (*playing_game State.init
  *)
  failwith "unimplemented"

let rec make_list lst acc=
  (*match lst with 
    |[]-> acc
    |h::t-> make_list t (h ^ "; " ^ acc)
  *)
  failwith "unimplemented"


let rec playing_game st found_wrds =
  (*if State.end_state st 
    then (print_endline("Game Over \n Your score: " ^ string_of_int State.score);
     exit 0)
    else 
    print_string ("Words found: \n");
    print_string (make_list found_wrds "");
    print_endline "Enter a word";
    try  
      match Command.parse(read_line ()) with
      |Quit ->
      |Score -> print_string (string_of_int (st.score))
      |Help -> 
      |Entry (guess) -> if List.mem guess found_wrds then
                           print_string "This word has been guessed.";
                           playing_game st found_wrds else
                        if Trie.contains st.words guess then
  *)
  failwith "unimplemented"



let main () =
  (*ANSITerminal.(print_string [red]
                  "\n Welcome to the Boggle Game engine.\n");
    print_endline "Please f to begin";
    print_string  "> "; 
    match read_line () with
    | "f"-> play_game ()
    | _ -> ()
  *)
  failwith "unimplemented"

