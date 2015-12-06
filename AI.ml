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

let ai_42 (l: word list): word option =
  match l with
  | h::t -> Some (List.fold_left score_comp h t)
  | []   -> None

let ai_d (l: word list) (d: int): word option =
  let comp w1 w2 = word_value w1 - word_value w2 in
  match l with
  | [] -> None
  | _  ->
    let len = List.length l in
    let (min, max) = ((d - 1)*len/5, d*len/5) in
    let ind = min +
                if (max - min) > 0 then
                  Random.int (max - min)
                else
                  0
    in
    Some (List.nth (List.sort comp l) ind)

let word_find (l: word list) (d: int): word option =
  match d with
  | 42 -> ai_42 l
  | 1 | 2 | 3 | 4 | 5 -> ai_d l d
  | _  -> failwith "invalid difficulty"

(* Return Some wd where wd is the best word that can be made from w and letters
 * if any word can be made. Return None if no word can be made. *)
let best_word (t: Trie.dict) (l: card list) (w: word) (d: int): word option =
  let gen_words = Trie.get_words t (List.rev_append l w) in
  let is_valid_word = is_valid_construct_ai w in
  word_find (List.filter is_valid_word gen_words) d

let best_word_build (t: Trie.dict) (l: card list) (d: int): word option =
  let gen_words = Trie.get_words t l in
  word_find gen_words d

let play_steal (g: game) (p: player) ((n, w, bw): steal_tup): game =
  if p.name = n then extend g w bw
  else steal g n w bw

let play_no_steal (g: game) (p: player) (t: Trie.dict) (d: int): game =
  match best_word_build t p.hand d with
  | Some w -> build g w
  | None   ->
    let g1 = draw_card g in
    match g1.players with
    | p1::_ ->
      let c = (List.nth p1.hand (List.length p1.hand - 1)) in
      discard_card g1 c
    | []    -> failwith "no_players"

let play_turn g t diff =
  (* For each word in pl.words, create a tuple containing pl.name, the best word
   * that can be generated from the word, and the word itself and then cons this
   * tuple to lst. *)
  let p = match g.players with
          | h::_ -> h
          | []   -> failwith "no_players" in
  let d = match diff with
          | 0 -> p.difficulty
          | _ -> diff in
  let flatten_words lst pl =
    let remap l w = (pl.name, w, best_word t p.hand w d)::l in
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
  match (steal_word_list, d) with
  | (h::t, 42) -> play_steal g p (List.fold_left score_comp_pair h t)
  | (h::t, _)  -> (match ai_d steal_word_list d with
                   | Some w -> play_steal g p w
                   | None   -> assert false)
  | ([], _)    -> play_no_steal g p t d