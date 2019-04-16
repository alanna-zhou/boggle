
<<<<<<< HEAD
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
  match trie with
  | Head  (children) -> Head (update_children children word)
  | _ -> add_word_help trie word

let add_words_from_file (filename:string) : t =
  failwith "unimplemented"

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
  let tail = String.sub word 1 ((String.length word) - 1) in 
  match children with
  | (Node (c, childs, is_word))::xs ->
  if c = curchar then contains_help (Node (c, childs, is_word)) word
  else check_char xs word
  | [Leaf]
  | [] -> false
  | _ -> failwith "Trie invalid"

let contains (trie:t) (word:string) : bool =
  match trie with
  | Head (children) -> check_char children word
  | _ -> contains_help trie word
  
=======
(* type t = Head of t list | Node of (string * t list) | Leaf  *)

type t = string list

(* let empty = Leaf  *)

let empty = []

(* let rec add_word trie word = 
   failwith "unimplemented" *)
(* match trie with
   | Head  (children) -> begin
    match children with
    | x::xs -> begin
        match x with
        | Node (c, children) -> if c = (String.sub word 0 1) then
            add_word x (String.sub word 1 (String.length word - 1))
        | Leaf -> add_word x word
      end
   end
   | Node (c, children) -> begin
    match children with
    | Leaf -> 

   end *)

let add_word (trie:t) (word:string) : t =
  if List.mem word trie then trie else word::trie
(* failwith "unimplemented" *)

let rec add_words (trie:t) (words:string list) : t =
  match words with 
  | [] -> trie
  | h::t -> add_words (add_word trie h) t 

let add_words_from_file (filename:string) : t =
  add_words empty ["RYE"; "TIE"; "DIE"]

let contains (trie:t) (word:string) : bool =
  List.mem word trie 

>>>>>>> acf2f8d21a276367dabb7d6b7e09a6346820560b
let to_list (trie:t) : string list =
  trie