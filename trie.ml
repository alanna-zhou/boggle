module Trie = struct

  type t = Head of t list | Node of (string * t list) | Leaf ;;

  let rec add_word trie word = 
    match trie with
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

      end