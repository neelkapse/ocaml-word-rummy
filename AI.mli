open GameState
open Player
open Toolbox

(* Given the game state, the dictionary, and a specific player,
   this function will 'think' of the best move for the player
   and play it.
   It returns the updated game state. *)
val play_turn : game -> Trie.dict -> game