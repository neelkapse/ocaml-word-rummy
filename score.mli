type card = char
type word = card list
exception No_Such_Card

(* Returns the point value for a specific card  *)
val card_value : card -> int

(* Returns the point value of a word *)
val word_value : word -> int