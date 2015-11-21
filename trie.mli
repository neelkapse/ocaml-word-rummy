(* Recursive trie data structure definition.
   The has_word variable might be unnecessary. *)
type node = {
  letter: char;
  links: node list;
  word: string;
  has_word: bool
}

(* A dictionary can be defined by its root node *)
type dict = {
  root: node
}

(* Inserts a given word into a dictionary, and 
   outputs the new dictionary *)
val insert : dict -> string -> dict

(* Given a file name, this function constructs a 
   dictionary using the words in the file *)
val construct : string -> dict

(* Checks whether a given word exists in a 
   dictionary *)
val has_word : dict -> string -> bool

(* Given a set of characters, this function will 
   find and output the best possible word that 
   can be constructed using some subset of the 
   characters. Returns None if no construction 
   is possible. *)
val get_word : dict -> char list -> string option
