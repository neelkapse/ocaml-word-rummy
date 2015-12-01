type card = char
type word = card list
exception No_Such_Card

(* Returns the point value for a specific card  *)
val card_value : card -> int

(* Returns the point value of a word *)
val word_value : word -> int

(* [is_valid_construct w1 w2] returns true if the extension of [w1] to [w2] is 
 * legal *)
val is_valid_construct : word -> word -> bool