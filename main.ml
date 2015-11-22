open GameState
open AI
open Score
open Trie

let create_random_deck num_cards = failwith "TODO"

let print_instructions () = failwith "TODO"

let create_player_list num_p num_ai = failwith "TODO"

let init () =
  print_string "Enter the number of human players: ";
  let num_players = int_of_string (read_line ()) in
  print_string "Enter the number of cpu players: ";
  let num_AI = int_of_string (read_line ()) in
  print_string "Enter the file name of your dictionary: ";
  let dict_file = read_line () in
  let dict = construct dict_file in
  let player_list = create_player_list num_players num_AI in
  let gs = { deck = create_random_deck 52;
             discarded = [];
             players = player_list
           }
  in
  (gs, dict)

let turn g d =
  let curr_player = List.hd g.players in
  Printf.printf "It is now %s's turn.\n\n" curr_player.name in
  let _ = print_string string_of_game g in
  let _ = print_string "\nWould you like to STEAL a word, BUILD a word, or
                        DRAW cards?\n" in
  let player_input = read_line() in
  failwith "REST TODO"



let (init_gs, init_dict) = init ()
let _ = turn init_gs init_dict