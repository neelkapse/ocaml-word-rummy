open GameState
open AI
open Score
open Trie
open Player

let create_random_deck num_cards =
  let letters = ['a';'a';'a';'a';'a';'a';'a';'a';'a';'b';'b';'c';'c';
                     'd';'d';'d';'d';'e';'e';'e';'e';'e';'e';'e';'e';'e';
                     'e';'e';'e';'f';'f';'g';'g';'g';'h';'h';'i';'i';'i';
                     'i';'i';'i';'i';'i';'i';'j';'k';'l';'l';'l';'l';'m';
                     'm';'n';'n';'n';'n';'n';'n';'o';'o';'o';'o';'o';'o';
                     'p';'p';'q';'r';'r';'r';'r';'r';'r';'s';'s';'s';'s';
                     't';'t';'t';'t';'t';'t';'u';'u';'u';'u';'v';'v';'w';
                     'w';'x';'y';'y';'z'] in
  let letters_with_weights = List.map (fun x -> (x, Random.int 500)) letters in
  let f x y = (snd x) - (snd y) in
  let sorted_weights = List.sort f letters_with_weights in
  List.map (fun x -> fst x) sorted_weights

let print_instructions () = print_string "TODO"

let create_player_list num_p num_ai =
  let rec create pl ai acc = match (pl, ai) with
    | (0,0) -> acc
    | (t,0) -> let new_player = { name = "Player " ^ (string_of_int t);
                                  hand = [];
                                  words = [];
                                  is_ai = false
                                }
                                in create (t-1) 0 (new_player::acc)
    | (t,v) -> let new_player = { name = "CPU " ^ (string_of_int v);
                                  hand = [];
                                  words = [];
                                  is_ai = true
                                }
                                in create t (v-1) (new_player::acc)
  in
  create num_p num_ai []

let rec replenish_hand_fst_player g =
  let num_cards = List.length (List.hd g.players).hand in
  if (num_cards < 7) then
    replenish_hand_fst_player (draw_card g)
  else
    g

let init () =
  print_string "Enter the number of human players: ";
  let num_players = int_of_string (read_line ()) in
  print_string "Enter the number of cpu players: ";
  let num_AI = int_of_string (read_line ()) in
  print_string "Enter the file name of your dictionary: ";
  let dict_file = read_line () in
  let dict = construct dict_file in
  let player_list = create_player_list num_players num_AI in
  let gs = { deck = create_random_deck 100;
             discarded = [];
             players = player_list
           }
  in
  (gs, dict)

let steal_turn g d =
  let old_string = print_string "Enter the word you wish to steal: ";
                   read_line () in
  let new_string = print_string "Please enter the new word you wish to form: ";
                   read_line () in
  failwith "TODO"

let build_turn g d =
  let new_string = print_string "Enter the new word you wish to build: ";
                   read_line () in
  failwith "TODO"

let draw_turn g d =
  let new_string = print_string "Enter the cards you wish to discard as
                                 a string without spaces or punctuation: ";
                   read_line () in
  failwith "TODO"

let extend_turn g d =
  let old_string = print_string "Enter the word you wish to extend: ";
                   read_line () in
  let new_string = print_string "Enter the new word you wish to form: ";
                   read_line () in
  failwith "TODO"

let print_final_results g = failwith "TODO"

let rec turn_minus_intro g d =
  let _ = print_string "\nWould you like to STEAL a word, BUILD a new word,
                        EXTEND one of your words or
                        DRAW cards?\n" in
  let player_input = String.uppercase (read_line()) in
  if player_input = "DRAW" then
    draw_turn g d
  else if player_input = "STEAL" then
    steal_turn g d
  else if player_input = "DRAW" then
    draw_turn g d
  else if player_input = "EXTEND" then
    extend_turn g d
  else
    print_string "Invalid input. Try again.\n";
    turn_minus_intro g d

let turn g d =
  if is_over g then
    print_final_results g
  else
    let g_after_draw = replenish_hand_fst_player g in
    let curr_player = List.hd g.players in
    Printf.printf "It is now %s's turn.\n\n" curr_player.name;
    let _ = print_string (string_of_game g_after_draw) in
    turn_minus_intro g_after_draw d



let (init_gs, init_dict) = init ()
let _ = turn init_gs init_dict