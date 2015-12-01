open Player

(* A game can be defined by the current deck, pile of 
 * discarded cards, the states of each player, and by
 * knowing whose turn it currently is. 
 *
 * RI:
 * 		deck: top of deck is head of list 
 * 		discarded: top of discard pile is head of list 
 * 		players: current player is at head of list
 *)
type game = {
  deck : card list;
  discarded : card list;
  players : player list;
}

(* Returns true if game is over (i.e. if the deck is empty) *)
val is_over : game -> bool

val rotate : game -> game

(* Perform word building operations *)
val extend : game -> word -> word -> game
val steal : game -> string -> word -> word -> game
val build : game -> word -> game

(* Performs a draw operation. Assumes that game is not over, and that current 
 * player is at head of list *)
val draw_card : game -> game

(* Performs a discard operation. Assumes that current player has just drawn, 
 * and is at head of list *)
val discard_card : game -> card -> game

val string_of_game : game -> string