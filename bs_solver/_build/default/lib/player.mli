type t =
  { id : int
  ; mutable hand_size : int
  ; mutable win_cycle : Card.t list
  ; mutable bluffs_completed : int
  ; mutable known_cards : Card.t list
  }
[@@deriving sexp_of, compare, fields]
