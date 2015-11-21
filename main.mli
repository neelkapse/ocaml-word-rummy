open GameState
open AI
open Score

(* Simulate the next turn of the game. This may involve 
   taking input for the user's move, or calling AI 
   functions, depending on whose move it is. The updated 
   game state is returned. This can be thought of as the
   REPL. *)
val next_turn : game -> dict -> game

(* Initializes the game state, after taking inputs for 
   game settings (eg. number of AI players, difficulty 
   level, etc) *)
val init : unit -> game -> dict