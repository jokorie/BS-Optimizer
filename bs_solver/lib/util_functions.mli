open! Core

val chop_win_seq : (Card.Rank.t * int) list -> (Card.Rank.t * int) list

val calc_win_cycle
  :  me:Player.t
  -> game_state:Game_state.t
  -> (Card.Rank.t * int) list

val cards_to_provide
  :  player:Player.t
  -> game_state:Game_state.t
  -> Card.Rank.t list
