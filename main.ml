open GameState
open AI
open Score
open Trie

let create_random_deck () = failwith "TODO"

let create_player_list num_p num_ai = failwith "TODO"

let init () =
  print_string "Enter the number of human players: ";
  let num_players = int_of_string (read_line ()) in
  print_string "Enter the number of cpu players: ";
  let num_AI = int_of_string (read_line ()) in
  print_string "Enter the file name of your dictionary";
  let dict_file = read_line () in
  let dict = construct dict_file in
  let gs = failwith "TODO" in
  (gs, dict)

let next_turn g d = failwith "TODO"