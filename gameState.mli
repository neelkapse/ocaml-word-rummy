open Player

(* A game can be defined by the current deck, pile of
 * discarded cards, the states of each player, and by
 * knowing whose turn it currently is.
 *
 * RI:
 *      deck: top of deck is head of list
 *      discarded: top of discard pile is head of list
 *      players: current player is at head of list
 *)
type game = {
  deck : card list;
  discarded : card list;
  players : player list;
}

(* Returns true if game is over (i.e. if the deck is empty) *)
val is_over : game -> bool

(* Rotates the player list, equivalent to a player making no move
 * Should only be used during initialization *)
val rotate : game -> game

(* Fills a player's hand to 7 cards *)
val replenish_hand : game -> game


(* Perform word building operations *)

(* [extend g w1 w2] extends the word [w1] owned by the current player to a word 
 * [w2] 
 *
 * Preconditions:
    the current player has ownership of the word [w1]
    the current player's hand contains all letters in [w2] that are not in [w1]
 *)
val extend : game -> word -> word -> game

(* [steal g s w1 w2] extends the word [w1] owned by the player named [s] to a 
 * word [w2], and places the resulting word in the ownership of the current 
 * player 
 * 
 * Preconditions:
    [s] owns the word [w1]
    the current player's hand contains all letters in [w2] that are not in [w1]
 *)
val steal : game -> string -> word -> word -> game

(* [build g w] places the word w on the board, in the current player's 
 * ownership. 
 *
 * Precondition: the current player's hand contains all letters in [w]
 *)
val build : game -> word -> game

(* [draw g] gives a card from the top of the deck to the current player. 
 * [discard] should be called immediately after this function, to ensure that 
 * no player has more than the hand limit of cards.
 * 
 * Precondition:  not (is_over)
 *)
val draw_card : game -> game

(* [discard g c] removes the card [c] from the current player's hand and puts 
 * it into the discard pile.
 * Warning: this does not replenish the hand, and should only be called 
 * immediately after [draw].
 * 
 * Preconditions: the current player's hand contains the card [c]
 *)
val discard_card : game -> card -> game

(* Returns a string tuple containing (board representation, current player's hand, deck size) *)
val string_of_game : game -> (string list * string * string)


