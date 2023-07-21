open! Core

type t =
  { id : int (* represent position as well *)
  ; mutable hand_size : int
  ; mutable win_cycle : Card.t list
  ; mutable bluffs_completed : int
  ; mutable my_cards : Card.t list
  }
[@@deriving sexp, compare, fields]

let card_on_turn num =
  match num % 13 with
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

let calc_win_cycle t ~(game_state : Game_state.t) =
  let current_turn = game_state.round_num + t.id in
  List.init 13 ~f:(fun cycle_count ->
    card_on_turn (current_turn + (game_state.player_count * cycle_count)))
;;
