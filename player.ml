type card = char
type word = card list

type player = {
  name: string;
  hand: card list;
  words: word list;
  difficulty: int
}

let rec string_of_char_list cs sep =
  match cs with
  | [] -> ""
  | h::t -> Bytes.make 1 h ^ sep ^ string_of_char_list t sep

let string_of_hand p = string_of_char_list p.hand " "

let string_of_player p =
  let f = (fun ws w -> ws ^ "\n\t\t" ^ (string_of_char_list w "")) in
  let words =
    match List.length p.words with
    | 0 -> ""
    | _ -> List.fold_left f "" p.words in
  words ^ "\n"
  (* "\t" ^ p.name ^ words ^ "\n" *)
