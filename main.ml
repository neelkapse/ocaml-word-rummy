open GameState
open AI
open Toolbox
open Trie
open Player

let print_result g =
  let open List in
  let open Printf in
  let rec create_score_list = function
    | [] -> []
    | h::t ->
      (fold_left (fun acc x -> acc + word_value x) 0 h.words)::(create_score_list t) in
  let _ = print_string (string_of_game g) in
  let score_list = create_score_list g.players in
  let max_score =
    match score_list with
    | [] -> failwith "no_players"
    | h::t -> fold_left (fun max x -> if x > max then x else max) h t in
  let _ = iter2
    (fun x y -> printf "%s :\t%d\n" x.name y) g.players score_list in
  let winners =
    filter (fun (_,x) -> x = max_score) (combine g.players score_list) |> split |> fst in
  match winners with
  | [] -> failwith "no_winner"
  | [x] -> printf "And...the winner is :\t%s!!! Congratulations!" x.name
  | h::t ->
    printf "And....the winners are: %s"
      (fold_left (fun acc x -> acc ^ ", " ^ x.name) h.name t)

let create_random_deck () =
  Random.self_init ();
  let letters_l = ['a';'a';'a';'a';'a';'a';'a';'a';'a';'b';'b';'c';'c';
                 'd';'d';'d';'d';'e';'e';'e';'e';'e';'e';'e';'e';'e';
                 'e';'e';'e';'f';'f';'g';'g';'g';'h';'h';'i';'i';'i';
                 'i';'i';'i';'i';'i';'i';'j';'k';'l';'l';'l';'l';'m';
                 'm';'n';'n';'n';'n';'n';'n';'o';'o';'o';'o';'o';'o';
                 'p';'p';'q';'r';'r';'r';'r';'r';'r';'s';'s';'s';'s';
                 't';'t';'t';'t';'t';'t';'u';'u';'u';'u';'v';'v';'w';
                 'w';'x';'y';'y';'z'] in
  let letters_k = letters_l @ letters_l in
  let letters = List.map (Char.uppercase) letters_k in
  let letters_with_weights = List.map (fun x -> (x, Random.int 500)) letters in
  let f x y = (snd x) - (snd y) in
  let sorted_weights = List.sort f letters_with_weights in
  List.map (fun x -> fst x) sorted_weights

let rec init_player_cards g num_players =
  if num_players = 0 then
    g
  else
    init_player_cards (g |> replenish_hand |> rotate) (num_players - 1)


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

let rec input_num_humans () =
  let () = print_string "Enter the number of human players: " in
  try
    let num_players = int_of_string (read_line()) in
    if num_players >= 0 then
      num_players
    else
      let () = print_string
                  "Number of human players cannot be negative, try again!\n" in
      input_num_humans ()
  with
  | _ -> let () = print_string
                    "Number of human players must be a number, try again!\n" in
         input_num_humans ()

let rec input_num_ai num_players =
  let () = print_string "Enter the number of CPU players: " in
  try
    let num_ai = int_of_string (read_line()) in
    if num_ai > 0 || (num_ai = 0 && num_players > 0) then
      num_ai
    else if num_ai = 0 && num_players = 0 then
      let () = print_string
                "Total number of players cannot be zero, try again!\n" in
      input_num_ai num_players
    else
      let () = print_string
                  "Number of CPU players cannot be negative, try again!\n" in
      input_num_ai num_players
  with
  | _ -> let () = print_string
                    "Number of CPU players must be a number, try again!\n" in
         input_num_ai num_players

let rec input_dictionary () =
  let () = print_string "Enter the file name of your dictionary: " in
  let filename = read_line() in
  try
    let _ = open_in filename in
    filename
  with
  | _ -> let () = print_string "That file wasn't found, try again!\n" in
         input_dictionary ()

let init () =
  print_string "\nWelcome to OCaml World Rummy v1.2\n\n";
  let num_players = input_num_humans () in
  let num_AI = input_num_ai num_players in
  let filename = input_dictionary () in
  let dictionary = Hashtbl.create 80_000 in
  let ic = open_in filename in
  let rec insert_words (ic : in_channel) : unit =
    try
      let s = String.uppercase (input_line ic) in
      let () = Hashtbl.add dictionary (string_to_word s) true in
      insert_words ic
    with
    | e -> () in
  let () = insert_words ic in
  let ai_trie = construct filename in
  let player_list = create_player_list num_players num_AI in
  let gs_without_cards = {
              deck = create_random_deck ();
              discarded = [];
              players = player_list
           }
  in
  let gs = init_player_cards gs_without_cards (num_players + num_AI)
  in
  (gs, ai_trie, dictionary)

let draw_turn g (tri_d, hash_d) =
  let after_draw = draw_card g in
  let drawn_hand = (List.hd after_draw.players).hand in
  let drawn_card = List.hd drawn_hand in
  Printf.printf "You just drew a card with letter %c.\n" drawn_card;
  print_string ("You must discard one card. Which letter would you like to " ^
                                                                  "discard?\n");
  let discard_letter_string = String.uppercase (read_line ()) in
  let f = fun x -> String.make 1 x = discard_letter_string in
  let is_valid = List.exists f drawn_hand in
  if is_valid then
    discard_card after_draw (String.get discard_letter_string 0)
  else
    let _ = print_string ("That was an invalid choice...as a result, " ^
                                         "you discarded the card you drew.\n") in
    discard_card after_draw (drawn_card)

let rec build_turn g (tri_d, hash_d) =
  let new_word = print_string "Enter the new word you wish to build: ";
                           string_to_word (String.uppercase (read_line ())) in
  let curr_player = List.hd g.players in
  let hand = curr_player.hand in
  if is_valid_build hash_d new_word hand then
    build g new_word
  else
    let _ = print_string "That was an invalid choice...let's try that again.\n" in
    (build_turn g (tri_d, hash_d))

let rec extend_turn g (tri_d, hash_d) =
  let curr_player = List.hd g.players in
  let curr_word_list = curr_player.words in
  print_string "Enter the word of yours you wish to extend: ";
  let old_word = string_to_word (String.uppercase (read_line ())) in
  print_string "Enter the new word you wish to form: ";
  let new_word = string_to_word (String.uppercase (read_line ())) in
  if (List.mem old_word curr_word_list) &&
            (is_valid_construct hash_d old_word new_word curr_player.hand) then
    extend g old_word new_word
  else
    let _ = print_string "That was an invalid choice...let's try that again.\n" in
    extend_turn g (tri_d, hash_d)

let rec steal_turn g (tri_d, hash_d) =
  print_string ("Enter the name of the player you wish to steal\nfrom " ^
                                        "(capitalization and spacing MATTERS): ");
  let name = read_line () in
  let finder = fun x -> (x.name = name) in
  if List.exists finder g.players then
    let curr_player = List.find finder g.players in
    let this_player = List.hd g.players in
    let curr_word_list = curr_player.words in
    print_string "Enter the word you wish to steal: ";
    let old_word = string_to_word (String.uppercase (read_line ())) in
    print_string "Enter the new word you wish to form: ";
    let new_word = string_to_word (String.uppercase (read_line ())) in
    if (List.mem old_word curr_word_list) &&
            (is_valid_construct hash_d old_word new_word this_player.hand) then
      steal g curr_player.name old_word new_word
    else
      let _ = print_string "That was an invalid choice...let's try that again.\n" in
      steal_turn g (tri_d, hash_d)
  else
    let _ = print_string "That was an invalid choice...let's try that again.\n" in
    steal_turn g (tri_d, hash_d)

let rec human_turn g (tri_d, hash_d) =
  print_string ("\nWould you like to STEAL a word, BUILD a new word,\n" ^
    "EXTEND one of your words or DRAW cards?\n> ");
  match String.uppercase (read_line()) with
    | "DRAW" -> draw_turn g (tri_d, hash_d)
    | "STEAL" -> steal_turn g (tri_d, hash_d)
    | "BUILD" -> build_turn g (tri_d, hash_d)
    | "EXTEND" -> extend_turn g (tri_d, hash_d)
    | _ -> print_string "Invalid input. Try again.\n"; human_turn g (tri_d, hash_d)

let ai_turn g (tri_d, hash_d) =
  print_string "The AI is thinking...\n";
  let new_gs = AI.play_turn g tri_d in
  print_string "The AI has made its turn.\n\n";
  new_gs

let rec turn g (tri_d, hash_d) =
  if is_over g then
    print_result g
  else
    let curr_player = List.hd g.players in
    print_string "\n______________________________________________________\n\n";
    Printf.printf "It is now %s's turn.\n\n" curr_player.name;
    print_string (string_of_game g);
    let new_gs =
      if curr_player.is_ai then
        ai_turn g (tri_d, hash_d)
      else
        human_turn g (tri_d, hash_d)
    in turn new_gs (tri_d, hash_d)

let (init_gs, init_trie, init_dict) = init ()
let _ = turn init_gs (init_trie, init_dict)