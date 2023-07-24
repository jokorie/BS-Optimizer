open! Core

type t =
  { mutable round_num : int
  ; player_count : int
  ; mutable pot_size : int
  }

val card_on_turn : t -> Card.Rank.t
