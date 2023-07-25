open! Core

type t =
  { mutable round_num : int
  ; player_count : int
  ; mutable pot : Card.t list
  ; all_players : All_players.t
  ; my_id : int
  }

[@@deriving fields, sexp]

let card_on_turn t =
  match t.round_num % 13 with
  | 1 -> (Ace : Card.Rank.t)
  | 2 -> Two
  | 3 -> Three
  | 4 -> Four
  | 5 -> Five
  | 6 -> Six
  | 7 -> Seven
  | 8 -> Eight
  | 9 -> Nine
  | 10 -> Ten
  | 11 -> Jack
  | 12 -> Queen
  | 0 -> King
  | _ -> failwith "Invalid round!"
;;
