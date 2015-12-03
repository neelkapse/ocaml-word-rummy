open GameState
open Player
open Toolbox

type steal_tup = string * word * word

(* Return the word with the higher score (return w2 if the scores are equal). *)
let score_comp (w1: word) (w2: word): word =
  if word_value w1 > word_value w2 then w1 else w2

(* Return the pair containing the word with the higher score (return t2 if the
 * scores are equal). *)
let score_comp_pair (t1: steal_tup) (t2: steal_tup): steal_tup =
  let ((_,_,w1), (_,_,w2)) = (t1, t2) in
  if word_value w1 > word_value w2 then t1 else t2

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
  | None   ->
    let g1 = draw_card g in
    match g1.players with
    | p1::_ -> discard_card g (List.nth p1.hand (List.length p1.hand - 1))
    | []    -> assert false

let play_turn (g: game) (p: player): game =
  (* For each word in pl.words, create a tuple containing pl.name, the best word
   * that can be generated from the word, and the word itself and then cons this
   * tuple to lst. *)
  let flatten_words lst pl =
    let remap l w = (pl.name, w, best_word p.hand w)::l in
    List.fold_left remap lst pl.words in
  (* If bword is Some bw, then generate the tuple (name, w, bw) and return the
   * tuple cons l. Otherwise, return l. *)
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