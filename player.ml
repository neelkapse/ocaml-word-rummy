type card = char
type word = card list

type player = {
  name: string;
  hand: card list;
  words: word list;
  is_ai: bool
}

let rec string_of_char_list cs sep =
  match cs with
  | [] -> ""
  | h::t -> (Bytes.make 1 h) ^ sep ^ string_of_char_list t sep

let string_of_hand p = string_of_char_list p.hand "  "

let string_of_player p =
  let f = (fun ws w -> ws ^ "\n\t\t" ^ (string_of_char_list w "")) in
  let words = List.fold_left f "" p.words in
  p.name ^ "\n\t\t" ^ words ^ "\n\n"
