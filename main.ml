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

let string_to_word s =
  let len = String.length s in
  let rec strip w i =
    if i = -1 then w
    else strip ((String.get s i)::w) (i - 1) in
  strip [] (len - 1)

let create_random_deck () =
  Random.self_init ();
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

let init () =
  print_string "Enter the number of human players: ";
  let num_players = int_of_string (read_line ()) in
  print_string "Enter the number of cpu players: ";
  let num_AI = int_of_string (read_line ()) in
  print_string "Enter the file name of your dictionary: ";
  let filename = read_line () in
  let dictionary = Hashtbl.create 80_000 in
  let ic = open_in filename in
  let rec insert_words (ic : in_channel) : unit =
    try
      let () = Hashtbl.add dictionary (string_to_word (input_line ic)) true in
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

let draw_turn g (tri_d, hash_d) = failwith "TODO"

let rec build_turn g (tri_d, hash_d) =
  let new_word = print_string "Enter the new word you wish to build: ";
                           string_to_word (String.uppercase (read_line ())) in
  let curr_player = List.hd g.players in
  let hand = curr_player.hand in
  if is_valid_build hash_d new_word hand then
    build g new_word
  else
    let _ = print_string "That was an invalid choice...let's try that again." in
    (build_turn g (tri_d, hash_d))

let extend_turn g (tri_d, hash_d) = failwith "TODO"

let steal_turn g (tri_d, hash_d) = failwith "TODO"

let rec human_turn g (tri_d, hash_d) =
  print_string ("\nWould you like to STEAL a word, BUILD a new word," ^
    " EXTEND one of your words or DRAW cards?\n");
  match String.uppercase (read_line()) with
    | "DRAW" -> draw_turn g (tri_d, hash_d)
    | "STEAL" -> steal_turn g (tri_d, hash_d)
    | "BUILD" -> build_turn g (tri_d, hash_d)
    | "EXTEND" -> extend_turn g (tri_d, hash_d)
    | _ -> print_string "Invalid input. Try again.\n";
                                                human_turn g (tri_d, hash_d)

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