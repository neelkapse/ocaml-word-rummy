open GameState
open AI
open Toolbox
open Trie
open Player

let print_game g =
  let open ANSITerminal in
  let rec print_players ps ws =
    match (ps, ws) with
    | ([], _) -> () 
    | (_, []) -> failwith "player_print error"
    | (p::pt, w::wt)  -> 
      print_string [Bold; yellow] ("\t" ^ p.name);
      print_string [] w;
      print_players pt wt in
  let (board, hand, deck_size) = string_of_game g in
  print_string [Bold] "\nDECK:"; 
  print_string [] (" There are " ^ deck_size ^ " cards left\n");
  print_string [Bold] "BOARD:\n";
  print_players g.players board; 
  print_string [cyan] "------------------------------------------------------";
  print_string [Bold] "\nHAND:\t";
  print_string [blue; on_white] hand;
  print_string [red] "\n______________________________________________________\n";
  ()

let print_result g =
  let open List in
  let open Printf in
  let open ANSITerminal in
  let rec create_score_list = function
    | [] -> []
    | h::t ->
      (fold_left (fun acc x -> acc + word_value x) 0 h.words)::(create_score_list t) in
  let _ = print_game g in
  let score_list = create_score_list g.players in
  let max_score =
    match score_list with
    | [] -> failwith "no_players"
    | h::t -> fold_left (fun max x -> if x > max then x else max) h t in
  let _ = iter2
    (fun x y -> printf [] "%s :\t%d\n" x.name y) g.players score_list in
  let winners =
    filter (fun (_,x) -> x = max_score) 
      (combine g.players score_list) |> split |> fst in
  match winners with
  | [] -> failwith "no_winner"
  | [x] -> 
    print_string [] "And...the winner is : ";
    print_string [Bold; yellow] x.name;
    print_string [] "!!! Congratulations!\n\n"
  | h::t ->
    print_string [] "And....the winners are: ";
    print_string [Bold; yellow] (fold_left (fun acc x -> acc ^ ", " ^ x.name) h.name t)

let create_random_deck num_players =
  let letters_l = ['a';'a';'a';'a';'a';'a';'a';'a';'a';'b';'b';'c';'c';
                 'd';'d';'d';'d';'e';'e';'e';'e';'e';'e';'e';'e';'e';
                 'e';'e';'e';'f';'f';'g';'g';'g';'h';'h';'i';'i';'i';
                 'i';'i';'i';'i';'i';'i';'j';'k';'l';'l';'l';'l';'m';
                 'm';'n';'n';'n';'n';'n';'n';'o';'o';'o';'o';'o';'o';
                 'p';'p';'q';'r';'r';'r';'r';'r';'r';'s';'s';'s';'s';
                 't';'t';'t';'t';'t';'t';'u';'u';'u';'u';'v';'v';'w';
                 'w';'x';'y';'y';'z'] in
  let letters_k =
    let rec make_letters num =
      if num <= 4 then
        letters_l
      else
        letters_l @ (make_letters (num_players - 4))
    in
    make_letters num_players
  in
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

let rec get_nth lst n =
  match lst with
  | [] -> failwith "Not possible"
  | h::t -> (if n = 0 then
              h
             else
              get_nth t (n - 1))

let pick_random index =
  let all_names = [["a"]; ["b"]; ["c"]; ["d"]; ["e"]; ["f"]] in
  let names = get_nth all_names index in
  let r = Random.int (List.length names) in
  get_nth names r

let get_ai_name difficulty =
  let index =
    match difficulty with
    | 1 -> 0
    | 2 -> 1
    | 3 -> 2
    | 4 -> 3
    | 5 -> 4
    | 42 -> 5
    | _ -> failwith "Invalid difficulty"
  in
  pick_random index

let create_player_list num_p num_ai ai_diffs =
  let rec create pl ai acc diffs = match (pl, ai) with
    | (0,0) -> acc
    | (t,0) -> let new_player = { name = "Player " ^ (string_of_int t);
                                  hand = [];
                                  words = [];
                                  difficulty = 0
                                }
                              in create (t-1) 0 (new_player::acc) diffs
    | (t,v) -> (match diffs with
                | [] -> failwith "Not possible"
                | head::tail ->
                 (let new_player = { name = get_ai_name head ^
                                            " (CPU " ^ (string_of_int v) ^ ")";
                                     hand = [];
                                     words = [];
                                     difficulty = head
                                   }
                  in create t (v-1) (new_player::acc) tail))
  in
  create num_p num_ai [] ai_diffs

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

let rec input_pre_dictionary () =
  let () = print_string "Do you want to use the default dictionary? (Y/N)\n" in
  let () = print_string "> " in
  let input = String.uppercase (read_line ()) in
  if String.length input = 0 then
    let () = print_string "Invalid input, try again!\n" in
    input_pre_dictionary ()
  else
    match String.get input 0 with
    | 'Y' -> "default_dict.txt"
    | 'N' -> input_dictionary ()
    | _ -> let () = print_string "Invalid input, try again!\n" in
           input_pre_dictionary ()

let rec input_ai_diffs num_AI counter =
  if counter > num_AI then
    []
  else
    try
      let () = print_string
              ("Enter difficulty level of CPU " ^ string_of_int (counter) ^
              " (1 - easiest, 5 - hardest): ") in
      let diff = int_of_string (read_line()) in
      if (diff < 1 || diff > 5) && diff <> 42 then
        let () = print_string "That's not a valid level, try again!\n" in
        input_ai_diffs num_AI counter
      else
        diff::(input_ai_diffs num_AI (counter + 1))
    with
    | _ ->
        let () = print_string "That's not a valid level, try again!\n" in
        input_ai_diffs num_AI counter

let init () =
  let open ANSITerminal in
  Random.self_init ();
  print_string [] "\n\n\n\n\n\n\n\n\n\n\n\n\n\n";
  print_string [Bold] "Welcome to OCaml Word Rummy v1.2!\n\n";
  print_string [] "\n\n\n\n\n\n";
  print_string [] ("NOTE: To abort a build, steal, or extend, enter a period as " ^
                                                        "the first input!\n\n\n\n\n");
  let num_players = input_num_humans () in
  let num_AI = input_num_ai num_players in
  let num_total_players = num_players + num_AI in
  let ai_diffs = input_ai_diffs num_AI 1 in
  let filename = input_pre_dictionary () in
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
  let player_list = create_player_list num_players num_AI ai_diffs in
  let gs_without_cards = {
              deck = create_random_deck num_total_players;
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
  if new_word = ['.'] then
    g
  else
    let curr_player = List.hd g.players in
    let hand = curr_player.hand in
    match is_valid_build hash_d new_word hand with
      | (true, true) -> build g new_word
      | (false, _) -> let _ = print_string ("That's not a valid word.\nTry "
                              ^ "again...\n") in (build_turn g (tri_d, hash_d))
      | _ -> let _ = print_string ("Insufficient cards in hand.\nTry " ^
                        "again...\n") in (build_turn g (tri_d, hash_d))

let rec extend_turn g (tri_d, hash_d) =
  let curr_player = List.hd g.players in
  let curr_word_list = curr_player.words in
  print_string "Enter the word of yours you wish to extend: ";
  let old_word = string_to_word (String.uppercase (read_line ())) in
  if old_word = ['.'] then
    g
  else
    let _ = print_string "Enter the new word you wish to form: " in
    let new_word = string_to_word (String.uppercase (read_line ())) in
    let old_word_exists = List.mem old_word curr_word_list in
    let (v1,v2,v3) = is_valid_construct hash_d old_word new_word
                                                            curr_player.hand in
    match (old_word_exists, v1, v2, v3) with
      | (true,true,true,true) -> extend g old_word new_word
      | (false,_,_,_) -> let _ = print_string ("The word you're trying to" ^
                      " extend is not one of your words.\nTry again.\n") in
                                                  extend_turn g (tri_d, hash_d)
      | (_,false,_,_) -> let _ = print_string ("The word you're trying to" ^
                                       " make is invalid.\nTry again.\n") in
                                                  extend_turn g (tri_d, hash_d)
      | (_,_,false,_) -> let _ = print_string ("The new word cannot be built " ^
                       "from the old word and your cards.\nTry again.\n") in
                                                  extend_turn g (tri_d, hash_d)
      | _ -> let _ = print_string ("The new word you want to create must" ^
            "contain all letters within the original word\nTry again.\n") in
                                                  extend_turn g (tri_d, hash_d)

let rec print_candidates candidates counter =
  match candidates with
  | [] -> ()
  | h::t -> print_string ((string_of_int counter) ^ ")\t" ^ h.name ^ "\n");
            print_candidates t (counter + 1)

let rec resolve_steal_conflict candidates =
  print_string (string_of_int (List.length candidates) ^ " players have that " ^
                "word on the board. Which one would you like to steal from?\n");
  print_candidates candidates 1;
  print_string ("Enter the number corresponding to the player you'd like to steal from.\n");
  print_string ("> ");
  try
    let input = int_of_string (read_line ()) in
    if (input < 1 || input > (List.length candidates)) then
      let () = print_string "That's an invalid number, try again!" in
      resolve_steal_conflict candidates
    else
      get_nth candidates (input - 1)
  with
  | _ -> print_string "That's an invalid number, try again!";
         resolve_steal_conflict candidates

let rec steal_turn g (tri_d, hash_d) =
  print_string ("Which word would you like to steal?\n> ");
  let word = String.uppercase (read_line ()) in
  if word = "." then
    g
  else
    let old_word = string_to_word word in
    let word_finder = fun x -> (x = old_word) in
    let player_finder = fun x -> (List.exists word_finder x.words) in
    let other_players =
      match g.players with
      | [] -> failwith "no_players"
      | h::t -> t
    in
    let candidates = List.filter player_finder other_players in
    match candidates with
    | [] -> print_string ("That word isn't on the board, try again!\n");
            steal_turn g (tri_d, hash_d)
    | h::t -> (let victim =
                if List.length t = 0 then
                  h
                else
                  resolve_steal_conflict candidates
              in
    print_string ("Which word would you like to form?\n> ");
    let word = String.uppercase (read_line ()) in
    let new_word = string_to_word word in
    let my_hand =
      let first_player =
        match g.players with
        | [] -> failwith "no_players"
        | h::t -> h
      in first_player.hand
    in
    let (v1,v2,v3) = is_valid_construct hash_d old_word new_word my_hand in
      match (v1, v2, v3) with
      | (true,true,true) -> steal g victim.name old_word new_word
      | (false,_,_) -> let _ = print_string ("The word you're trying to" ^
                                       " make is invalid.\nTry again.\n") in
                                                    steal_turn g (tri_d, hash_d)
      | (_,false,_) -> let _ = print_string ("The new word cannot be built " ^
                       "from the old word and your cards.\nTry again.\n") in
                                                    steal_turn g (tri_d, hash_d)
      | _ -> let _ = print_string ("The new word you want to create must" ^
            "contain all letters within the original word\nTry again.\n") in
                                                    steal_turn g (tri_d, hash_d))

let rec check_turn g hash_d =
  print_string ("Enter the word that you'd like to check: \n> ");
  let word = String.uppercase (read_line ()) in
  let word_c = string_to_word word in
  let score = Toolbox.word_value word_c in
  let valid = Hashtbl.mem hash_d word_c in
  let () =
    if valid = false then
      print_string ("That's not a word!\n")
    else
      print_string
        (word ^ " will give you " ^ (string_of_int score) ^ " points!\n")
  in
  let rec another_input g hash_d =
    print_string ("Check another word? (Y/N)\n> ");
    let another = String.uppercase (read_line ()) in
    if String.length another = 0 then
      let () = print_string "Invalid input, try again\n" in
      another_input g hash_d
    else
      match String.get another 0 with
      | 'Y' -> check_turn g hash_d
      | 'N' -> g
      | _ -> let () = print_string "Invalid input, try again!\n" in
             another_input g hash_d
  in another_input g hash_d


let rec human_turn g (tri_d, hash_d) =
  let open ANSITerminal in
  print_string [] "\nTo ";
  print_string [Bold] "steal" ;
  print_string [] " a word, enter ";
  print_string [Bold] "S";
  print_string [] ".\nTo ";
  print_string [Bold] "build";
  print_string [] " a word, enter ";
  print_string [Bold] "B";
  print_string [] ".\nTo ";
  print_string [Bold] "extend";
  print_string [] " one of your words, enter ";
  print_string [Bold] "E";
  print_string [] ".\nTo ";
  print_string [Bold] "draw";
  print_string [] " a card, enter ";
  print_string [Bold] "D";
  print_string [] ".\nTo check the ";
  print_string [Bold] "score";
  print_string [] " of a word, enter ";
  print_string [Bold] "C";
  print_string [] ".\n> ";
  let input = String.uppercase (read_line ()) in
  if String.length input = 0 then
    let () = print_string [] "Invalid input, try again!\n" in
    human_turn g (tri_d, hash_d)
  else
    match String.get input 0 with
      | 'D' -> draw_turn g (tri_d, hash_d)
      | 'S' -> steal_turn g (tri_d, hash_d)
      | 'B' -> build_turn g (tri_d, hash_d)
      | 'E' -> extend_turn g (tri_d, hash_d)
      | 'C' -> check_turn g hash_d
      | _ -> print_string [] "Invalid input, try again!\n";
                                                      human_turn g (tri_d, hash_d)

let ai_turn g (tri_d, hash_d) =
  print_string "The AI is thinking...\n";
  let new_gs = AI.play_turn g tri_d 0 in
  print_string "The AI has made its turn.\n\n";
  new_gs

let rec turn g (tri_d, hash_d) =
  let open ANSITerminal in
  if is_over g then
    print_result g
  else
    let curr_player = List.hd g.players in
    print_string [red] "\n______________________________________________________\n\n";
    print_string [] "It is now ";
    print_string [Bold] curr_player.name;
    print_string [] "'s turn.";
    print_game g;
    let new_gs =
      if curr_player.difficulty <> 0 then
        ai_turn g (tri_d, hash_d)
      else
        human_turn g (tri_d, hash_d)
    in turn new_gs (tri_d, hash_d)

let (init_gs, init_trie, init_dict) = init ()
let _ = turn init_gs (init_trie, init_dict)