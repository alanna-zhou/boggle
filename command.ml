
type command =
  |Start
  |Quit
  |Score 
  |Help

exception Empty

exception Malformed

let parse str = 
  match (String.trim str) with
  |""-> Empty
  |"start"->Start
  |"quit"->Quit
  |"score"-> Score
  |"help"-> Help
  |_-> Invalid
