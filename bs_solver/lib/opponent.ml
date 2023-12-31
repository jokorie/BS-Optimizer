open! Core

type t =
  { id : int (* represent position as well *)
  ; mutable hand_size : int
  ; mutable win_cycle : Card.t list
  ; mutable bluffs_completed : int
  ; mutable known_cards : Card.t list
  }
[@@deriving sexp, compare, fields]
