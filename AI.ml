open GameState
open Player

let ai_trie = Trie.construct "SixtyK.txt"

(* Return true if gen_word contains all cards in steal_word and gen_word is
 * longer than steal_word. Return false otherwise. *)
let contains_word1 (steal_word: word) (gen_word: word): bool =
  (* If w contains card c, return w without card c. Otherwise, return an empty
   * list. *)
  let remove_char w c =
    let rec remove nw ow =
      match ow with
      | h::t -> if h = c then List.rev_append nw t
                else remove (h::nw) t
      | []   -> [] in
    remove [] w in
  (* If [] then either gen_word does not contain a card from steal_word or
   * gen_word is the same length as steal_word. *)
  match List.fold_left remove_char gen_word steal_word with
  | [] -> false
  | _  -> true

(* Return true if gen_word contains all cards in steal_word and gen_word is
 * longer than steal_word. Return false otherwise. *)
let contains_word2 (steal_word: word) (gen_word: word): bool =
  (* Increments the value in arr at the index corresponding to card c. *)
  let inc arr c =
    let i = Char.code c - 65 in
    arr.(i) <- (arr.(i) + 1) in
  (* Return true if the all values in arr1 are less than or equal to the
   * corresponding values in arr2. Return false otherwise. The lengths of arr1
   * and arr2 must be equal to len. *)
  let comp_arr arr1 arr2 len =
    let rec comp c =
      if c = len then true
      else if arr1.(c) > arr2.(c) then false
      else comp (c + 1) in
    comp 0 in
  if List.length steal_word < List.length gen_word then
    let (steal_count, gen_count) = (Array.make 26 0, Array.make 26 0) in
    let (inc_steal, inc_gen) = (inc steal_count, inc gen_count) in
    let _ = (List.iter inc_steal steal_word, List.iter inc_gen gen_word) in
    comp_arr steal_count gen_count 26
  else false

(* Return the word with the higher score (return word2 if the scores are
 * equal). *)
let score_comp (word1: word) (word2: word): word =
  if Score.word_value word1 > Score.word_value word2 then word1 else word2

(* Return Some wd where wd is the best word that can be made from w and letters
 * if any word can be made. Return None if no word can be made. *)
let best_word (letters: card list) (w: word): word option =
  let gen_words = Trie.get_words ai_trie (w@letters) in
  let is_valid_word = contains_word1 w in
  match List.filter is_valid_word gen_words with
  | []   -> None
  | h::t -> Some (List.fold_left score_comp h t)

let play_not_steal (g: game) (p: player): game =
  failwith "unimplemented"

let play_steal (g: game) (p: player) (lst: (string * word) list): game =
  failwith "unimplemented"

let play_turn (g: game) (p: player): game =
  let best_player_word =
    let opt_best_word w1 w2 =
      match (w1, w2) with
      | (_, None)          -> w1
      | (None, _)          -> w2
      | (Some w3, Some w4) -> Some (score_comp w3 w4) in
    List.fold_left opt_best_word None in
  let opt_filter l (name, b_word) =
    match b_word with
    | None   -> l
    | Some w -> (name, w)::l in
  let steal_word_list = g.players
    |> List.map (fun p -> (p.name, p.words))
    |> List.map (fun (name, words) -> (name, List.map (best_word p.hand) words))
    |> List.map (fun (name, b_words) -> (name, best_player_word b_words))
    |> List.fold_left opt_filter [] in
  match steal_word_list with
  | [] -> play_not_steal g p
  | _  -> play_steal g p steal_word_list