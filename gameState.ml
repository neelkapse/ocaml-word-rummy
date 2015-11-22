(* HELPER FUNCTIONS *)

(* Removes one copy of the specified card from the given card list *)
let rec remove_card (hand: card list) (c: card): card list =
  match hand with
  | [] -> []
  | h::t ->
    if h = c 
    then t
    else h :: remove_card t c

(* Returns a tuple containing (drawn cards, remaining deck) *)
let rec get_cards (deck: card list) (i: int) : (card list * card list) =
  match (deck, i) with
  | (_, 0) -> ([], deck)
  | ([], _) -> ([], [])
  | (h::t, _) ->
    let (sub_hand, sub_deck) = get_cards t (i-1) in
    (h::sub_hand, sub_deck)

(* PUBLIC METHODS *)

let is_over g =
  match g.deck with
  | [] -> true
  | _ -> false

let extend game w1 w2 =
  match game.players with
  | [] -> failwith "no_players"
  | p::_ -> steal game p w1 w2

let steal game p w1 w2 =
  failwith "unimplemented"

let build game word =
  let rec remove_word hand w =
    match w with
    | [] -> []
    | h::t -> update_hand (remove_card hand h) t in
  match game.players with
  | [] -> failwith "no_players"
  | p::players_t ->
    let (new_cards, deck') = get_cards (game.deck) (List.length word) in
    let hand' = new_cards :: (remove_word (p.hand) word) in
    let words' = word :: p.words in
    let p' = {p with hand = hand'; words = words'} in
    let players' = p' :: players_t in
    {game with deck = deck'; players = players'}

let draw_card game =
  match (game.deck, players) with 
  | ([], _) -> failwith "deck_is_empty"
  | (card::d', p::players_t) ->
    let p' =  {p with hand = card :: (p.hand)} in
    let players' = p' :: players_t in
    {game with deck = d'; players = players'}

let discard_card game card =
  match game.players with
  | [] -> failwith "no_players"
  | p::players_t -> 
    let discarded' = card :: game.discarded in
    let p' = {p with hand = (remove_card p.hand card)} in
    let players' = p' :: players_t in
    {game with discarded = discarded'; players = players'}

