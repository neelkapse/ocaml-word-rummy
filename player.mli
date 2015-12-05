(* Temporary type definitions, may change based on implementations *)
type card = char
type word = card list

(* A player is defined by a name, the current hand of cards, the list of
 * words that the player possesses, and a difficulty level. The difficulty
 * of human players is set to be 0. It is only meaningful for AI players.
 *
 * RI: [name] must be unique for each player
 *)
type player = {
  name: string;
  hand: card list;
  words: word list;
  difficulty: int
}

val string_of_hand : player -> string
val string_of_player : player -> string