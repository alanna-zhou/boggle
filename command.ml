module Command = struct 

  type command =
    |Start
    |Quit
    |Score 
    |Help

  exception Empty

  exception Invalid

  let parse str = 
    match (String.trim str) with
    |""-> raise Empty
    |"#start"->Start
    |"#quit"->Quit
    |"#score"-> Score
    |"#help"-> Help
    |_-> raise Invalid

end