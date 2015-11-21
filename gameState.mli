open Player

(* A game can be defined by the current deck, pile of 
   discarded cards, the states of each player, and by
   knowing whose turn it currently is. *)
type game = {
  deck : card list;
  discarded : card list;
  players : player list;
  turn : player
}

(* Returns total current score of player *)
val score : player -> int

(* Returns true if game is over (i.e. if the deck is empty) *)
val is_over : game -> bool

(* Perform word building operations *)
val extend : game -> player -> word -> word -> game
val steal : game -> player -> player -> word -> word -> game
val build : game -> player -> word -> game

(* Perform draw/discard operations *)
val draw_card : game -> player -> game
val discard_card : game -> player -> card -> game

