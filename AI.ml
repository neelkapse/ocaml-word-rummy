open GameState
open Player

type steal_tup = string * word * word

let ai_trie = Trie.construct "SixtyK.txt"

(* Return true if gen_word contains all cards in steal_word and gen_word is
 * longer than steal_word. Return false otherwise. *)
let contains_word (steal_word: word) (gen_word: word): bool =
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
let contains_word (steal_word: word) (gen_word: word): bool =
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

(* Return the word with the higher score (return w2 if the scores are equal). *)
let score_comp (w1: word) (w2: word): word =
  if Score.word_value w1 > Score.word_value w2 then w1 else w2

(* Return the pair containing the word with the higher score (return t2 if the 
 * scores are equal). *)
let score_comp_pair (t1: steal_tup) (t2: steal_tup): steal_tup =
  let ((_,_,w1), (_,_,w2)) = (t1, t2) in
  if Score.word_value w1 > Score.word_value w2 then t1 else t2

(* Return Some wd where wd is the best word that can be made from w and letters
 * if any word can be made. Return None if no word can be made. *)
let best_word (letters: card list) (w: word): word option =
  let gen_words = Trie.get_words ai_trie (List.rev_append letters w) in
  let is_valid_word = contains_word w in
  match List.filter is_valid_word gen_words with
  | h::t -> Some (List.fold_left score_comp h t)
  | []   -> None

let best_word_build (letters: card list): word option = best_word [] letters

let play_steal (g: game) (p: player) ((n, w, bw): steal_tup): game =
  if p.name = n then extend g w bw
  else steal g n w bw

let play_no_steal (g: game) (p: player): game =
  match best_word_build p.hand with
  | Some w -> build g w
  | None   -> failwith "unimplemented"

let play_turn (g: game) (p: player): game =
  let flatten_words lst pl =
    let remap l w = (pl.name, w, best_word p.hand w)::l in
    List.fold_left remap lst pl.words in
  let opt_filter l (name, w, bword) =
    match bword with
    | Some bw -> (name, w, bw)::l
    | None    -> l in
  let steal_word_list = g.players
    |> List.fold_left flatten_words []
    |> List.fold_left opt_filter [] in
  match steal_word_list with
  | h::t -> play_steal g p (List.fold_left score_comp_pair h t)
  | []   -> play_no_steal g p