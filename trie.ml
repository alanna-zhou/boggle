
type t = Head of t list | Node of (string * t list * bool) | Leaf 

let empty = Head ([Leaf])

let rec add_word_help trie word =
  let curchar = String.sub word 0 1 in 
  let tail = String.sub word 1 ((String.length word) - 1) in
  match trie with
  | Node (c, children, _) -> Node (c, update_children children tail,
                                   String.length tail = 1)
  | Leaf -> Node (curchar, update_children [Leaf] tail,
                  String.length tail = 1)
  | _ -> failwith "Function does not accept Head as input"
and update_children children word =
  if word = "" then children else
    let curchar = String.sub word 0 1 in 
    let tail = String.sub word 1 ((String.length word) - 1) in 
    match children with
    | (Node (c, childs, _))::xs -> if c = curchar then
        (add_word_help (Node (c, childs, String.length tail = 1)) word)::xs else
        (Node (c, childs, String.length tail = 1))::(update_children xs word)
    | [Leaf]
    | [] -> [(Node (curchar, update_children [Leaf] tail, 
                    String.length tail = 1))]
    | _ -> failwith "Function does not accept Head as input"

let add_word trie word = 
  let word = String.lowercase_ascii word in
  match trie with
  | Head  (children) -> Head (update_children children word)
  | _ -> add_word_help trie word

let rec add_words trie words =
  match words with
  | [] -> trie
  | x::xs -> add_words (add_word trie x) xs

let rec read_words trie channel =
  let to_add = try input_line channel with
    | End_of_file -> "\n"
  in
  if to_add = "\n" then trie else
    let trie = add_word trie to_add in
    read_words trie channel;;

let add_words_from_file (filename:string) : t =
  read_words empty (open_in filename)


let rec contains_help trie word =
  let tail = String.sub word 1 ((String.length word) - 1) in 
  match trie with
  | Node (c, children, is_word) -> if String.length tail = 1 then
      is_word && check_char children tail else
      check_char children tail
  | Leaf -> false
  | Head (_) -> failwith "Function does not accept Head as input"
and check_char children word =
  if word = "" then true else
    let curchar = String.sub word 0 1 in 
    match children with
    | (Node (c, childs, is_word))::xs ->
      if c = curchar then contains_help (Node (c, childs, is_word)) word
      else check_char xs word
    | [Leaf]
    | [] -> false
    | _ -> failwith "Trie invalid"

let contains (trie:t) (word:string) : bool =
  let word = String.lowercase_ascii word in 
  match trie with
  | Head (children) -> check_char children word
  | _ -> contains_help trie word

let rec contains_prefix_help trie pref =
  let tail = String.sub pref 1 ((String.length pref) - 1) in 
  match trie with
  | Node (c, children, _) -> if String.length tail = 1 then
      check_pref children tail else
      check_pref children tail
  | Leaf -> false
  | Head (_) -> failwith "Function does not accept Head as input"
and check_pref children pref =
  if pref = "" then true else
    let curchar = String.sub pref 0 1 in 
    match children with
    | (Node (c, childs, is_word))::xs ->
      if c = curchar then contains_help (Node (c, childs, is_word)) pref
      else check_char xs pref
    | [Leaf]
    | [] -> false
    | _ -> failwith "Trie invalid"

let contains_prefix trie pref =
  let pref = String.lowercase_ascii pref in 
  match trie with
  | Head (children) -> check_pref children pref
  | _ -> contains_help trie pref

let rec to_list_help trie acc curword =
  match trie with
  | Node (c, children, is_word) -> let curword = curword ^ c in 
    if is_word then
      children_to_list children (curword::acc) curword else
      children_to_list children acc curword
  | Leaf -> acc
  | _ -> failwith "invalid"
and children_to_list children acc curword =
  match children with
  | x::xs -> children_to_list xs (to_list_help x acc curword) curword
  | [] -> acc

let to_list (trie:t) : string list =
  match trie with
  | Head (children) -> children_to_list children [] ""
  | _ -> failwith "Invalid Trie"
