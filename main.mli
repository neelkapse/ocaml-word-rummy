open GameState
open AI
open Toolbox
open Trie

(* Simulate the turn of the game. This may involve
   taking input for the user's move, or calling AI
   functions, depending on whose move it is. The updated
   game state is returned. This can be thought of as the
   REPL. *)
val turn : game -> (dict * ((word, bool) Hashtbl.t)) -> unit

(* Initializes the game state and dictionary, after taking inputs for
   game settings (eg. number of AI players, difficulty
   level, etc) *)
val init : unit -> (game * dict * ((word, bool) Hashtbl.t))