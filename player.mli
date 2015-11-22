(* Temporary type definitions, may change based on implementations *)
type card = char
type word = card list

(* A player is defined by the current hand of cards, the list of
   words that the player possesses, and whether the player is an
   AI or not. *)
type player = {
  	name: string;
	hand: card list;
	words: word list;
	is_ai: bool
}

val string_of_player : player -> string