open Trie

TEST "empty dictionary" =
  let d = create () in
  let words = get_words d ['a'] in
  List.length words = 0

let d = construct "SixtyK.txt"

let rec insertWordsIntoList (ic : in_channel) (l : string list) : string list =
  try
    let word = input_line ic in
    print_string word;
    insertWordsIntoList ic (word::l)
  with
  | e -> l

let lst = insertWordsIntoList (open_in "SixtyK.txt") []