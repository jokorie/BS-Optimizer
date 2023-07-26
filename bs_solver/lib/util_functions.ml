open! Core

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
  (*Given a win sequence, if we dont have cards at the end of our win
    sequence, just eliminate that part.*)
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

let calc_win_cycle ~(me : Player.t) ~(game_state : Game_state.t) =
  (* id should start at 0 if round starts at 1 *)
  (*Calculates the list of the cards we need to provide according to the
    cards we have in our hand.*)
  let current_turn = game_state.round_num + me.id in
  let full_cycle =
    List.init 13 ~f:(fun cycle_count ->
      let rank =
        card_on_turn (current_turn + (game_state.player_count * cycle_count))
      in
      rank, Hashtbl.find_exn me.cards rank)
  in
  chop_win_seq full_cycle
;;

let cards_to_provide ~(player : Player.t) ~(game_state : Game_state.t) =
  (* Functionaly to calculate a list of the cards one needs to provide
     throughout the next 13 rounds of the game. id should start at 0 if round
     starts at 1 *)
  let current_turn = game_state.round_num + player.id in
  List.init 13 ~f:(fun cycle_count ->
    card_on_turn (current_turn + (game_state.player_count * cycle_count)))
;;
