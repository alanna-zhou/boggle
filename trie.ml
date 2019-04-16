
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

let combine (trie1:t) (trie2:t) : t =
  trie1 @ trie2

let contains (trie:t) (word:string) : bool =
  List.mem word trie 

let to_list (trie:t) : string list =
  trie