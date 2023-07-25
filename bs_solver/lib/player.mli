open! Core
type t =
  { id : int
  ; mutable hand_size : int
  ; mutable win_cycle : Card.t list
  ; mutable bluffs_completed : int
  (* ; mutable my_cards : (Card.Rank.t, int) Hashtbl *)
  }
[@@deriving sexp, fields]

