type t =
  { id : int
  ; mutable hand_size : int
  ; mutable win_cycle : Card.t list
  ; mutable bluffs_completed : int
  ; mutable my_cards : (Card.Rank.t, int) Hashtbl.t
  }
[@@deriving fields]

val calc_win_cycle : t -> game_state:Game_state.t -> (Card.Rank.t * int) list
