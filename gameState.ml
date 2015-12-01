open Player

type game = {
  deck : card list;
  discarded : card list;
  players : player list;
}

(* HELPER FUNCTIONS *)

(* Removes one copy of the specified ['a] from the given ['a] list *)
let rec remove_element (xs: 'a list) (x: 'a): 'a list =
  match xs with
  | [] -> []
  | h::t ->
    if h = x
    then t
    else h :: remove_element t x

(* Returns a tuple containing (drawn cards, remaining deck) *)
let rec get_cards (deck: card list) (i: int) : (card list * card list) =
  match (deck, i) with
  | (_, 0) -> ([], deck)
  | ([], _) -> ([], [])
  | (h::t, _) ->
    let (sub_hand, sub_deck) = get_cards t (i-1) in
    (h::sub_hand, sub_deck)

(* PUBLIC METHODS *)

let is_over (g: game) =
  match g.deck with
  | [] -> true
  | _ -> false

let steal game name w1 w2 =
  let rec steal_from_player ps =
    match ps with
    | [] -> []
    | p::t ->
      if p.name = name then
        {p with words = (remove_element p.words w1)} :: t
      else
        p :: (steal_from_player t) in
  match game.players with
  | [] -> failwith "no_players"
  | p::players_t ->
    let p' = {p with words = w2 :: p.words} in
    let players' = steal_from_player (p'::players_t) in
    {game with players = players'}

let extend game w1 w2 =
  match game.players with
  | [] -> failwith "no_players"
  | p::_ -> steal game (p.name) w1 w2

let build game word =
  let rec remove_word hand w: card list =
    match w with
    | [] -> []
    | h::t -> remove_word (remove_element hand h) t in
  match game.players with
  | [] -> failwith "no_players"
  | p::players_t ->
    let (new_cards, deck') = get_cards (game.deck) (List.length word) in
    let hand' = new_cards @ (remove_word (p.hand) word) in
    let words' = word :: p.words in
    let p' = {p with hand = hand'; words = words'} in
    let players' = p' :: players_t in
    {game with deck = deck'; players = players'}

let draw_card game =
  match (game.deck, game.players) with
  | ([], _) -> failwith "deck_is_empty"
  | (_, []) -> failwith "no_players"
  | (card::d', p::players_t) ->
    let p' =  {p with hand = card :: (p.hand)} in
    let players' = p' :: players_t in
    {game with deck = d'; players = players'}

let discard_card game card =
  match game.players with
  | [] -> failwith "no_players"
  | p::players_t ->
    let discarded' = card :: game.discarded in
    let p' = {p with hand = (remove_element p.hand card)} in
    let players' = p' :: players_t in
    {game with discarded = discarded'; players = players'}

let string_of_game g =
  failwith "unimplemented"
