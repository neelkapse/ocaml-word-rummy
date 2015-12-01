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

(* Return true if gen_word contains all cards in steal_word and gen_word is
 * longer than steal_word. Return false otherwise. *)
let is_valid_construct (steal_word: word) (gen_word: word): bool =
  (* If w contains card c, return w without card c. Otherwise, return an empty
   * list. *)
  let remove_char w c =
    let rec remove nw ow =
      match ow with
      | h::t -> if h = c then List.rev_append nw t
                else remove (h::nw) t
      | []   -> [] in
    remove [] w in
  (* If [] then either gen_word does not contain a card from steal_word or
   * gen_word is the same length as steal_word. *)
  match List.fold_left remove_char gen_word steal_word with
  | [] -> false
  | _  -> true

(* Return true if gen_word contains all cards in steal_word and gen_word is
 * longer than steal_word. Return false otherwise. *)
let is_valid_construct (steal_word: word) (gen_word: word): bool =
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
  if List.length steal_word < List.length gen_word then
    let (steal_count, gen_count) = (Array.make 26 0, Array.make 26 0) in
    let (inc_steal, inc_gen) = (inc steal_count, inc gen_count) in
    let _ = (List.iter inc_steal steal_word, List.iter inc_gen gen_word) in
    comp_arr steal_count gen_count 26
  else false
