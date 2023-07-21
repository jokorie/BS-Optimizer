open! Core

type t =
  { mutable round_num : int
  ; player_count : int
  ; mutable pot : Card.t list (* accumulated cards*)
  ; all_players : Player.t Int.Table.t
  }

val card_on_turn : t -> Card.Rank.t
