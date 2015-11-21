open GameState
open Trie
open Score

(* Given the game state, the dictionary, and a specific player, 
   this function will 'think' of the best move for the player 
   and play it.
   It returns the updated game state. *)
val play_turn : game -> player -> game