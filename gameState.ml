
let is_over = function
  | [] -> true
  | _ -> false

let update_player (ps: (string * player) list) (p: player) 
(id: string) : player list =
  List.map (fun (s,x) -> if s = id then p else x) ps 

let draw_card g p =
  match g.deck with 
  | [] -> failwith "deck_is_empty"
  | card::d' ->
    let p' =  {p with hand = card :: (p.hand)} in
    let players' = update_player g.players p' in
    let discarded' = card :: discarded in 
    {game with deck = d'; discarded = discarded'; players = players'}