open! Core

type t =
  { mutable round_num : int
  ; player_count : int
  ; mutable pot : Card.t list (* accumulated cards*)
  ; all_players : All_players.t
  ; my_id : int
  }
  [@@deriving fields, sexp]

val card_on_turn : t -> Card.Rank.t
