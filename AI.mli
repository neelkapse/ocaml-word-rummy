open GameState
open Player
open Toolbox

(* Given the game state, the dictionary, and a difficulty level, this function
 * will 'think' of the best move for the player and play it and then returns the
 * updated game state. *)
val play_turn : game -> Trie.dict -> int -> game