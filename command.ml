  
type command =
  |Start
  |Quit
  |Score 
  |Help
  |Entry of string

exception Empty

exception Invalid

let parse str = 
  match (String.trim str) with
  |""-> raise Empty
  |"#quit" -> Quit
  |"#score" -> Score
  |"#help" -> Help
  |x -> Entry (x)