open! Core

type t =
  { mutable round_num : int
  ; player_count : int
  ; mutable pot : Card.t list (* accumulated cards*)

  }

val card_on_turn : t -> Card.Rank.t
