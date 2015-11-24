type card = char
type word = card list

type player = {
  name: string;
  hand: card list;
  words: word list;
  is_ai: bool
}

let string_of_player p =
  failwith "unimplemented" 
