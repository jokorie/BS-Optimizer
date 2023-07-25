open! Core


let assess_calling_bluff ~(all_players : All_players.t) ~(card: Card.Rank.t) =
  let num_card_i_have = Hashtbl.find_exn all_players card in




