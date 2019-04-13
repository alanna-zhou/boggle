(** [t] is the type of a Trie, initially will be only a string list list*)
type t

(** [add_word trie word] is [trie] after [word] is inserted into it*)
val add_word : t -> string -> t

(** [add_words_from_file filename] is the trie generated by adding
  all the words from file with name [filename] to an empty trie*)
val add_words_from_file : string -> t

(** [contains trie word] is whether or not [word] is contained within
  [trie]*)
val contains : t -> string -> bool

(** [to_list trie] is [trie] in the form of an ordered string list*)
val to_list : t -> string list