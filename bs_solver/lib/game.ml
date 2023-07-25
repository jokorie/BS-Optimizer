open! Core

let game_init
  ~player_count
  ~(my_pos : int)
  ~(my_cards : (Card.Rank.t, int) Hashtbl.t)
  =
  (*we dont know the position until the person with the ace of spades has
    acted*)
  let all_players = Int.Table.create () in
  let _ =
    List.init player_count ~f:(fun player_id ->
      let cards = if my_pos = player_id then my_cards else Card.Rank.Table.create() in
      Hashtbl.set
        all_players
        ~key:player_id
        ~data:
          { Player.id = player_id
          ; hand_size =
              (if player_id < 52 % player_count
               then (52 / player_count) + 1
               else 52 / player_count)
          ; win_cycle = []
          ; bluffs_completed = 0
          ; cards
          })
  in
  let game_state =
    { Game_state.round_num = 1; player_count; pot = []; all_players }
  in
  game_state
;;
