open String
open Char
open Score

(* Recursive trie data structure definition.
   A node is defined by the character it holds,
   the word it holds, and the list of nodes it
   is the parent of. *)
type node =
   | Sentinel
   | Node of char * (node list) * string

(* A dictionary can be defined by its root node *)
type dict = node

(* Creates an empty dictionary *)
let create () : dict =
   Node (' ', [], "")

(* HELPER FUNCTIONS FOR INSERT *)
let charSortFunction (a : char) (b : char) : int =
  if a > b then 1
  else if a = b then 0
  else -1

let sortCharList (lst : char list) : char list =
  List.sort charSortFunction lst

let assignCharToNode (ch : char) (n : node) : node =
   match n with
   | Sentinel -> failwith "Cannot assign character to Sentinel Node"
   | Node (_, links, word) -> Node (ch, links, word)

let assignWordToNode (word : string) (n : node) : node =
   match n with
   | Sentinel -> failwith "Cannot assign word to Sentinel Node"
   | Node (ch, links, _) -> Node (ch, links, word)

let assignLinksToNode (links : node list) (n : node) : node =
   match n with
   | Sentinel -> failwith "Cannot assign links to Sentinel Node"
   | Node (ch, _, word) -> Node (ch, links, word)

let getLetter (n : node) : char =
   match n with
   | Sentinel -> failwith "Cannot get letter from Sentinel Node"
   | Node (ch, _, _) -> ch

let getLinks (n : node) : node list =
   match n with
   | Sentinel -> failwith "Cannot get links from Sentinel Node"
   | Node (_, links, _) -> links

let getWord (n : node) : string =
   match n with
   | Sentinel -> failwith "Cannot get word from Sentinel Node"
   | Node (_, _, word) -> word

let overwrite (d : dict) (subd : node) : dict =
   let subLetter = getLetter subd in
   let rec removeLinkTo (c : char) (lst: node list) =
      match lst with
      | [] -> []
      | h::t -> (if (getLetter h) = c then
                  t
                 else
                  h::(removeLinkTo c t))
   in
   let listWithout = removeLinkTo subLetter (getLinks d) in
   let listWith = subd::listWithout in
   assignLinksToNode listWith d

let getLink (c : char) (d : dict) : node =
   let rec searchLinks (lst : node list) : node =
      match lst with
      | [] -> (let newNode = create () in
               assignCharToNode c newNode)
      | h::t ->
            (if (getLetter h) = c then
               h
             else
               searchLinks t)
   in searchLinks (getLinks d)

let hasLink (c : char) (d : dict) : bool =
  let rec searchLinks (lst : node list) : bool =
    match lst with
    | [] -> false
    | h::t ->
          (if (getLetter h) = c then
             true
           else
              searchLinks t)
  in searchLinks (getLinks d)

let rec collectWords (cards: char list) (d : dict) : char list list =
  let nodeWord = getWord d in
  let currentList =
     if nodeWord <> "" then
      [string_to_word nodeWord]
     else
      []
  in
  let rec loopChars (lst : char list) (prev : char) : char list list =
    match lst with
    | [] -> []
    | h::t -> (if h = prev then
                loopChars t h
               else
                if hasLink h d then
                  let nextNode = getLink h d in
                  List.append (collectWords t nextNode) (loopChars t h)
                else
                  loopChars t h)

  in
  List.append currentList (loopChars cards ' ')

(* Inserts a given word into a dictionary, and
   outputs the new dictionary *)
let insert (dictionary : dict) (word : string) : dict =
   let word = String.lowercase word in
   let charlist = string_to_word word in
   let sorted = sortCharList charlist in

   let rec explore (word : string) (chars : char list) (d : dict) : dict =
      match chars with
      | [] -> assignWordToNode word d
      | h::t -> let linkToH = getLink h d in
                let result = explore word t linkToH in
                overwrite d result

   in explore word sorted dictionary

(* Given a file name, this function constructs a
   dictionary using the words in the file *)
let construct (filename : string) : dict =
  let ic = open_in filename in
  let d = create () in
  let rec insertWords (ic : in_channel) (d : dict) : dict =
    try
      let word = input_line ic in
      insertWords ic (insert d word)
    with
    | e -> print_endline (Printexc.to_string e); d
  in insertWords ic d

(* Given a set of characters, this function will
   find and output all the possible valid words that
   can be made with any subset of the set. No order
   is guaranteed. If no words can be made, an empty
   list is returned. Only words present in the given
   dictionary will be returned. *)
let rec get_words (dictionary : dict) (cards : char list) : char list list =
  let cards = sortCharList cards in
  let cards = List.map lowercase cards in
  collectWords cards dictionary