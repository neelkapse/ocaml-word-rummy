(* Recursive trie data structure definition.
   A node is defined by the character it holds,
   the word it holds, and the list of nodes it
   is the parent of. *)
type node =
   | Root
   | Node of char * (node list) * string

(* A dictionary can be defined by its root node *)
type dict = node

(* Creates an empty dictionary *)
val create : unit -> dict

(* Inserts a given word into a dictionary, and
   outputs the new dictionary *)
val insert : dict -> string -> dict

(* Given a file name, this function constructs a
   dictionary using the words in the file *)
val construct : string -> dict

(* Given a set of characters, this function will
   find and output all the possible valid words that
   can be made with any subset of the set. No order
   is guaranteed. If no words can be made, an empty
   list is returned. Only words present in the given
   dictionary will be returned. *)
val get_words : dict -> char list -> char list list
