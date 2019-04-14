  type t = Head of t list | Node of (string * t list) | Leaf 

  let empty = Leaf 

  (* let rec add_word_help trie word = 
    if word = "" then Leaf else
    let curchar = String.sub word 0 1 in
    let tail = String.sub word 1 ((String.length word) - 1) in 
      match trie with
      | Node (c, children) -> add_char children word
      | Leaf -> Node (curchar, [add_word_help Leaf tail]) 
      | _ -> failwith "Function does not accept Head as input"
  and rec add_char children word =
    if word = "" then Leaf else
    let curchar = String.sub word 0 1 in
    let tail = String.sub word 1 ((String.length word) - 1) in  
    match children with
    | (Node (c, childs))::xs -> 
    if c = curchar then (add_word_help (Node (c, childs)) tail)::xs
    else (Node (c, childs))::(add_char xs word)
    | [Leaf] ->
    | [] -> Node (curchar, add_word_help Leaf tail)
    | _ -> failwith "Trie invalid"
*) 
  (* let add_word trie word = 
    match trie with
    | Head  (children) -> add_char children word
    | _ -> add_word_help trie word *)
    

let add_word (trie:t) (word:string) : t =
  failwith "unimplemented"

let add_words_from_file (word:string) : t =
  failwith "unimplemented"

let contains (trie:t) (word:string) : bool =
  failwith "unimplemented"

let to_list (trie:t) : string list =
  failwith "unimplemented"