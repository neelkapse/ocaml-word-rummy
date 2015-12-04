type card = char
type word = card list
exception No_Such_Card

(* Returns the point value of a word *)
val word_value : word -> int

(* Converts a string to a word *)
val string_to_word : string -> word

(* Converts a word to a string *)
val word_to_string : word -> string

(* [is_valid_build d w h] returns [(a, b)]. [a] is true if word [w] is a valid
 * word in [d] and false otherwise. [b] is true if [w] can be built from hand
 * [h]. *)
val is_valid_build : (card list, bool) Hashtbl.t -> word -> card list ->
  bool * bool

(* [is_valid_construct d w1 w2 h] returns [(a, b, c)]. [a] is true if word [w2]
 * is a valid word in [d] and false otherwise. [b] is true if [w2] can be built
 * from word [w1] and hand [h] and false otherwise. [c] is true if [w2] extends
 * [w1] (that is if [w2] contains all cards in [w1] and length [w2] > length
 * [w1]). *)
val is_valid_construct : (card list, bool) Hashtbl.t -> word -> word ->
  card list -> bool * bool * bool

(* [is_valid_construct_ai w1 w2] returns true if word [w2] can be built from
 * [w1] and [w2] is not a permutation of [w1] (that is, it ensures that length
 * [w2] > length [w1]) *)
val is_valid_construct_ai : word -> word -> bool