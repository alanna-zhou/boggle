open Board
open Command
open State

type game = {
  state : State.t;
}

let () = main ()