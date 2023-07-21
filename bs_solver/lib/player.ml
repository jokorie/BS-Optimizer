open! Core

type t =
  { (* maybe have two cases: Me & Opponent*)
    id : int (* represent position as well *)
  ; mutable hand_size : int
  ; mutable win_cycle : Card.t list
  ; mutable bluffs_completed : int
  ; mutable my_cards : (Card.Rank.t, int) Hashtbl.t
      (*remember to initialize all ranks as 0 in hashtbl*)
  }
[@@deriving fields]

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

let chop_win_seq sequence =
  let seq, _ =
    List.fold
      sequence
      ~init:([], [])
      ~f:(fun (built, pending) (rank, count) ->
        match count with
        | 0 -> built, pending @ [ rank, count ]
        | _ -> built @ pending @ [ rank, count ], [])
  in
  seq
;;

let calc_win_cycle t ~(game_state : Game_state.t) =
  (* id should start at 0 if round starts at 1 *)
  let current_turn = game_state.round_num + t.id in
  let full_cycle =
    List.init 13 ~f:(fun cycle_count ->
      let rank =
        card_on_turn (current_turn + (game_state.player_count * cycle_count))
      in
      rank, Hashtbl.find_exn t.my_cards rank)
  in
  chop_win_seq full_cycle
;;
