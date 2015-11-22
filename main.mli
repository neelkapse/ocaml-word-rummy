open GameState
open AI
open Score
open Trie

(* Simulate the turn of the game. This may involve
   taking input for the user's move, or calling AI
   functions, depending on whose move it is. The updated
   game state is returned. This can be thought of as the
   REPL. *)
val turn : game -> dict -> unit

(* Initializes the game state and dictionary, after taking inputs for
   game settings (eg. number of AI players, difficulty
   level, etc) *)
val init : unit -> game * dict