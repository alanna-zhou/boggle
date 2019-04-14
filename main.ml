open Board
open Command
open State

type game = {
  state : State.t;
}
let play_game () =
  failwith "unimplemented"
  (* playing_game State.init *)

let update_state (st:State.t)=
  failwith "unimplemented"
  (* print_endline "Enter a word";
  let input = read_line in
  if Trie.contains st.words input 
  then  *)

let rec playing_game state=
  failwith "unimplemented"
  (* if State.end_state state 
  then (print_endline("Game Over \n Your score: " ^ string_of_int State.score);
     exit 0)
  else playing_game (update_state state) *)

let main () =
  failwith "unimplemented"
  (* ANSITerminal.(print_string [red]
                  "\n Welcome to the Boggle Game engine.\n");
  print_endline "Please f to begin";
  print_string  "> "; 
    match read_line () with
  | "f"-> play_game ()
  | _ -> () *)
