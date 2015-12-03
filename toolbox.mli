type card = char
type word = card list
exception No_Such_Card

(* Returns the point value for a specific card  *)
val card_value : card -> int

(* Returns the point value of a word *)
val word_value : word -> int

(* Converts a string to a word *)
val string_to_word : string -> word

(* Converts a word to a string *)
val word_to_string : word -> string

(* [is_valid_word d w] returns true if [w] is in set [d] *)
val is_valid_word : (card list, bool) Hashtbl.t -> word -> bool

(* [is_sub_word w1 w2] returns true if [w1] can be constructed from [w2] *)
val is_sub_word : word -> word -> bool

(* [is_valid_build d w h] returns true if word [w] is a valid word in [d] and if
 * it can be built from hand [h] *)
val is_valid_build : (card list, bool) Hashtbl.t -> word -> card list -> bool

(* [is_valid_construct d w1 w2 h] returns true if word [w2] is a valid word in
 * [d] and if it can be built from word [w1] and hand [h] and that [w2] is not
 * a permutation of [w1] (that is, it ensures that length [w2] > length [w1]) *)
val is_valid_construct : (card list, bool) Hashtbl.t -> word -> word ->
  card list -> bool

(* [is_valid_construct_ai w1 w2] returns true if word [w2] can be built from
 * [w1] and [w2] is not a permutation of [w1] (that is, it ensures that length
 * [w2] > length [w1]) *)
val is_valid_construct_ai : word -> word -> bool