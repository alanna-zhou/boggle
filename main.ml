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

let format_color (board:Board.t) (size:size) (word:string): unit =
  print_string " ";
  let rec tborder count =
    match count with 
    |0 -> ()
    |n -> print_string"_"; tborder (count-1) in ignore(tborder (2*size-1));
  print_string("\n|");
  let node_color_lst = nodes_and_colors word board in
  let rec helper lst () i left=
    let n = (i mod size) in 
    match lst with 
    | [] -> ()
    | (letter, color)::t -> 
      if left=1 then
        begin match color with 
          | Green ->if n<> 0 then helper t (ANSITerminal.(print_string [green; Underlined] ((Char.escaped letter) ^ " "))) (i+1) (left)
            else ANSITerminal.(print_string [green; Underlined] ((Char.escaped letter)))
          | Red -> if n<> 0 then helper t (ANSITerminal.(print_string [red; Underlined] ((Char.escaped letter) ^ " "))) (i+1) (left)
            else ANSITerminal.(print_string [red; Underlined] ((Char.escaped letter)))
          | White ->if n<> 0 then helper t (ANSITerminal.(print_string [white; Underlined] ((Char.escaped letter) ^ " "))) (i+1) (left)
            else ANSITerminal.(print_string [white; Underlined] ((Char.escaped letter)))
        end
      else
        begin match color with 
          | Green -> if n <> 0 then helper t (ANSITerminal.(print_string [green] ((Char.escaped letter)^ " "))) (i+1) (left)
            else helper t (ANSITerminal.(print_string [green] (Char.escaped letter));print_string "|\n|") (i+1) (left-1)
          | Red -> if n <> 0 then helper t (ANSITerminal.(print_string [red] ((Char.escaped letter)^ " "))) (i+1) (left)
            else helper t (ANSITerminal.(print_string [red] (Char.escaped letter));print_string "|\n|") (i+1) (left-1)
          | White -> if n <> 0 then helper t (ANSITerminal.(print_string [white] ((Char.escaped letter)^ " "))) (i+1) (left)
            else helper t (ANSITerminal.(print_string [white] (Char.escaped letter));print_string "|\n|") (i+1) (left-1)
        end

  in (helper node_color_lst () 1 size);
  print_string "|"

let rec prompt_board_file () =
  try 
    let file = read_line() in file
  with 
  |Failure x-> ANSITerminal.(print_string [red] "Invalid size, try again.");
    prompt_board_file ()

and prompt_board_type game_number leaderboard () =
  print_endline "\nWhat kind of board would you like?";
  print_string  "\nType s for Standard, r for Random, or c for Custom. "; 
  match  read_line () with
  |"s" -> begin 
      print_string "\nDo you want to create a board with customized die or use\
                    built in standard boards? Type b for builtin, and cd for \
                    custom die. ";
      match read_line () with 
      |"b" -> begin
          print_string "\nWould you like the board to be a 4x4 or 5x5? ";
          let s = prompt_board_size () in 
          if s =4 || s = 5 then
            playing_game (Unix.time() +. 90.) 
              (State.init (Board.generate (Standard (s))) 
                 (if game_number = 0 then [] else leaderboard)) []
              game_number ""
          else ANSITerminal.(print_string [red] "\nInvalid entry";
                             prompt_board_type game_number leaderboard ()); 
        end
      |"cd"-> begin 
          try 
            print_string "\nWhat size board does this custom die correspond to? \
                          Entry must be less than 20 and greater than 4. ";
            (let s = prompt_board_size () in 
             print_string "\nEnter the file name of your custom die: ";
             let f = prompt_board_file() in
             playing_game (Unix.time() +. 90.) 
               (State.init (Board.generate (Custom_die(f, s))) 
                  (if game_number = 0 then [] else leaderboard)) []
               game_number "")
          with 
          |InvalidFile x -> ANSITerminal.(print_string [red] 
                                            (x ^ " is not a valid file name"));
            prompt_board_type game_number leaderboard ()
          |InvalidSize s -> ANSITerminal.
                              (print_string [red] ((string_of_int s) ^ 
                                                   " is not an allowed \
                                                    size, or the file \
                                                    does not correspond to \
                                                    the inputted size. "));
            prompt_board_type game_number leaderboard ()
        end
      |_ -> ANSITerminal.(print_string [red] "\nInvalid entry"; 
                          prompt_board_type game_number leaderboard ());
    end
  |"r" -> print_string "\nWhat size board would you like? For example, \
                        entering 10 will create a 10x10 board. Entry must be\
                        less than 20. ";
    let s = prompt_board_size () in 
    if s < 21 && s > 6
    then playing_game (Unix.time() +. 90.) 
        (State.init (Board.generate (Random(s))) 
           (if game_number = 0 then [] else leaderboard)) []
        game_number ""
    else ANSITerminal.(print_string [red] "\nRandom size must be greater than \
                                           or equal to 6, and less than or \
                                           equal to 20."; 
                       prompt_board_type game_number leaderboard ());

  |"c" -> begin 
      try 
        print_string "\nWhat size board are you uploading? Entry must be less \
                      than 20 and greater than 4. ";
        (let s = prompt_board_size () in 
         print_string "\nEnter the file name of your custom board: " ;
         let f = prompt_board_file() in
         playing_game (Unix.time() +. 90.) 
           (State.init (Board.generate (Custom_board(f, s))) 
              (if game_number = 0 then [] else leaderboard)) []
           game_number "")
      with 
      |InvalidFile x -> ANSITerminal.(print_string [red]
                                        (x ^ " is not a valid file name"));
        prompt_board_type game_number leaderboard ()
      |InvalidSize s -> ANSITerminal.
                          (print_string [red] ((string_of_int s)
                                               ^ " is not of the allowed \
                                                  size, or the file does not \
                                                  correspond to the inputted \
                                                  size. "));
        prompt_board_type game_number leaderboard ()
    end
  |_-> ANSITerminal.(print_string [red] "\nInvalid entry"; 
                     prompt_board_type game_number leaderboard ());


and playing_game time (st: State.t) (found_wrds: string list) game_number lguess=
  let () = Random.self_init () in 
  if is_game_over time
  then 
    end_game game_number st found_wrds time
  else begin
    try 
      format_color (State.board st) (Board.size (State.board st)) (lguess);
      print_string ("\nWords found: " ^ 
                    (make_list found_wrds "") ^ "\nEnter a word: ");
      match (Command.parse(read_line ())) with
      (*|Quit -> print_string "hi"; end_game game_number st*)
      |Score -> print_string ("\nYour score: " ^ string_of_int (State.score st));
        playing_game time st found_wrds game_number ""
      |Quit -> end_game game_number st found_wrds time
      |Leaderboard -> print_leaderboard (leaderboard st); 
        playing_game time st found_wrds game_number ""
      |Hint -> failwith "unimplemented"
      |Help -> print_string "\nTo play a word, enter that word.\
                             \nTo see your current score, enter #score.\
                             \nTo quit/restart game, enter #quit.\
                             \nFor a hint, enter #hint.\
                             \nTo see the leaderboard, enter #leaderboard.\
                             \nTo see instructions, enter #help.
                             \nIf you want to input custom boards, or custom \
                             die, you can upload txt files for those. \
                             \n\nFor custom die, see 4x4.txt as an example. \
                             Line number x contains the 6 sided configurations\
                             for each die on position number x on the board. \
                             These are capital letters not separated by any \
                             space. For a 4x4 board, you must have 16 lines \
                             corresponding to the 16 die. \n\n\
                             For custom boards, see board1.txt as an example \
                             txt file. The board is drawn out with no spaces \
                             between characters. The number of lines in the \
                             file should correspond to the dimension of the \
                             board. \n";
        playing_game time st found_wrds game_number ""
      |Entry (guess) -> 
        ignore(clear 0);
        if is_game_over time then end_game game_number st found_wrds time
        else if List.mem guess found_wrds then playing_game time st found_wrds 
            game_number guess
        else if not (Board.is_valid_word guess (State.board st)) 
        then raise (Failure guess)
        else begin
          let new_state = State.update st guess in 
          playing_game time (new_state) (guess :: found_wrds) game_number guess
        end
    with 
    | Failure x -> ignore(clear 0);
      ANSITerminal.(print_string [red] (x));
      print_string (" is not a valid input. \n"); 
      playing_game time st found_wrds game_number ""
    |Empty -> ignore(clear 0); 
      ANSITerminal.(print_string [red] "\nEntry is empty, choose another word.");
      playing_game time st found_wrds game_number ""


  end 

(** [end_game] ends the game.  *)
and end_game game_number st wrds time=
  ANSITerminal.(print_string [red] "\nGame Over"); 
  print_string ("\nYour score: ");
  ANSITerminal.(print_string [green](string_of_int (State.score st))); 
  print_string ("\nWords found.\n");
  print_list wrds;

  print_string ("\nWords missed.\n");
  print_list (unfound wrds (Board.get_possible_words (State.board st)) []);
  print_string ("\nAverage time between words: ");
  print_float ((min (90. -. (time-. Unix.time ())) 90. ) 
               /. (float (List.length wrds)));
  let new_leaderboard = add_leaderboard (leaderboard st) ([score st]) 
      (size (board st)) []  in
  let () = print_leaderboard new_leaderboard in 
  (prompt_end game_number (new_leaderboard) ())

and print_list lst =
  match lst with
  |[]-> ()
  |h::t-> if t = [] then (print_string (h); print_list t)
    else print_string (h ^ ", "); print_list t


and unfound found total acc=
  match total with
  |[]->acc
  |h::t-> if List.mem h found then unfound found t acc
    else unfound found t (h::acc)

(** [prompt_end] asks for user input on whether or not they'd like to 
    continue playing.  *)
and prompt_end game_number leaderboard () =
  print_string "\nPlay again? y/n : ";
  match read_line () with 
  |"y" -> prompt_board_type (game_number + 1) leaderboard () 
  |"n" -> () 
  |_ -> ANSITerminal.(print_string [red]"Not a valid input."); 
    prompt_end game_number leaderboard ()

and is_game_over time =
  Unix.time () >= time


let word_blitz_art () =
  print_string "WW               WW                                   dd      \
               \          BBBBBB      lll             tt                 !!\n";
  print_string "WW               WW                                   dd      \
               \          BB   BB      ll           tttttt               !!\n";
  print_string " WW             WW                                    dd      \
               \          BB   BB      ll     ii      tt                 !!\n";
  print_string " WW      W      WW      ooooo      rr rrr       ddddd dd      \
               \          BBBBBB       ll             tt      zzzzzz     !!\n";
  print_string "  WW    WWW    WW     oo     oo    rrr        dd     ddd      \
               \          BB   BB      ll    iii      tt         zz      !!\n";
  print_string "  WW   WW WW   WW    oo       oo   rr         dd      dd      \
               \          BB   BB      ll     ii      tt        zz         \n";
  print_string "   WW WW   WW WW      oo     oo    rr         dd     ddd      \
               \          BB   BB      ll     ii      tt       zz        !!\n";
  print_string "    WWW     WWW         ooooo      rr           ddddd dd      \
               \          BBBBBB        ll   iiii      ttt    zzzzzz     !!\n\n"



let main () =
  ignore (clear 0);
  print_string "Welcome to \n\n";
  word_blitz_art ();
  print_string "Form and enter words contained on the \
                board by connecting letters horizontally, vertically, or \
                diagonally.  At any time, type #help for gameplay instructions.\
                You can choose a board of your desired size, and configure a \
                board the way you want. You cannot use a board element more \
                than once on the board. Type #hint for a hint, but do know \
                that you have a maximum of 3 hints - each hint will lead to a \
                small score deduction. \n";
  prompt_board_type 0 [] ()

let () = main ()
