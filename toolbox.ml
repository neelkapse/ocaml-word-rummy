type card = char
type word = card list
exception No_Such_Card

let card_value c =
  match c with
  | 'A' -> 1
  | 'B' -> 3
  | 'C' -> 3
  | 'D' -> 2
  | 'E' -> 1
  | 'F' -> 4
  | 'G' -> 2
  | 'H' -> 4
  | 'I' -> 1
  | 'J' -> 8
  | 'K' -> 5
  | 'L' -> 1
  | 'M' -> 3
  | 'N' -> 1
  | 'O' -> 1
  | 'P' -> 3
  | 'Q' -> 10
  | 'R' -> 1
  | 'S' -> 1
  | 'T' -> 1
  | 'U' -> 1
  | 'V' -> 4
  | 'W' -> 4
  | 'X' -> 8
  | 'Y' -> 4
  | 'Z' -> 10
  | _   -> raise No_Such_Card

let word_value = List.fold_left (fun v c -> v + card_value c) 0

let string_to_word s =
  let len = String.length s in
  let rec strip w i =
    if i = -1 then w
    else strip ((String.get s i)::w) (i - 1) in
  strip [] (len - 1)

let word_to_string w =
  let len = List.length w in
  String.init len (fun i -> List.nth w i)

let is_valid_word d w =
  Hashtbl.mem d w

let is_sub_word (sub_word: word) (super_word: word): bool =
  (* Increments the value in arr at the index corresponding to card c. *)
  let inc arr c =
    let i = Char.code c - 65 in
    arr.(i) <- (arr.(i) + 1) in
  (* Return true if the all values in arr1 are less than or equal to the
   * corresponding values in arr2. Return false otherwise. The lengths of arr1
   * and arr2 must be equal to len. *)
  let comp_arr arr1 arr2 len =
    let rec comp c =
      if c = len then true
      else if arr1.(c) > arr2.(c) then false
      else comp (c + 1) in
    comp 0 in
  let (sub_count, super_count) = (Array.make 26 0, Array.make 26 0) in
  let (inc_steal, inc_gen) = (inc sub_count, inc super_count) in
  let _ = (List.iter inc_steal sub_word, List.iter inc_gen super_word) in
  comp_arr sub_count super_count 26

let is_valid_construct_ai steal_word gen_word =
  (List.length steal_word < List.length gen_word) &&
  is_sub_word steal_word gen_word

let is_valid_build dict build_word hand =
  is_valid_word dict build_word && is_sub_word build_word hand

let is_valid_construct dict steal_word gen_word hand =
  is_valid_word dict gen_word &&
  is_sub_word gen_word (steal_word@hand) &&
  is_valid_construct_ai steal_word gen_word