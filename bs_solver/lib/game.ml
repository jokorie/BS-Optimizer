open! Core
open! In_channel

let declare_player_count () =
  print_endline "Please specify how many players are in the round ";
  let player_count = In_channel.input_line_exn stdin in
  Int.of_string player_count
;;

let declare_my_pos () =
  print_endline
    "Please specify your seating index at the table clockwise from the 0th \
     player (the player with the Ace of Spades): ";
  let my_pos = In_channel.input_line_exn stdin in
  Int.of_string my_pos
;;

let declare_my_cards ~my_pos ~player_count =
  let hand_size =
    if my_pos < 52 % player_count
    then (52 / player_count) + 1
    else 52 / player_count
  in
  (* print_s[%message (hand_size:int)]; *)
  let my_cards = My_cards.init () in
  let _ =
    List.init hand_size ~f:(fun _ ->
      print_endline
        "Please specify the card you received in any order with the Rank \
         Suit notation \n\
        \ e.g. 2s - representing the Two of Spades";
      let card_input_string = In_channel.input_line_exn stdin in
      let card = Card.of_string card_input_string in
      My_cards.add_card my_cards ~card)
  in
  my_cards
;;

let game_init () =
  (*we dont know the position until the person with the ace of spades has
    acted*)
  let player_count = declare_player_count () in
  let my_pos = declare_my_pos () in
  let my_cards = declare_my_cards ~my_pos ~player_count in
  let all_players = Int.Table.create () in
  let _ =
    List.init player_count ~f:(fun player_id ->
      assert (player_count > 0);
      let cards =
        if my_pos = player_id then my_cards else My_cards.init ()
      in
      Hashtbl.set
        all_players
        ~key:player_id
        ~data:
          { Player.id = player_id
          ; hand_size =
              (if player_id < 52 % player_count
               then (52 / player_count) + 1
               else 52 / player_count)
          ; bluffs_completed = 0
          ; cards
          })
  in
  let game_state =
    { Game_state.round_num = 0
    ; player_count
    ; pot = []
    ; all_players
    ; my_id = my_pos
    }
  in
  print_s [%message (game_state : Game_state.t)];
  game_state
;;

let end_processes game =
  ignore game;
  print_endline "Wow game is over"
;;

let my_moves game =
  let player = Game_state.whos_turn game in
  player.hand_size <- player.hand_size - 1;
  print_s [%message "Cards left after move: " (player.hand_size : int)];
  print_endline "I made a move"
;;

let opp_moves game =
  let player = Game_state.whos_turn game in
  player.hand_size <- player.hand_size - 1;
  print_s [%message "Cards left after move: " (player.hand_size : int)];
  print_endline "Opp made a move"
;;

let rec play_game ~(game : Game_state.t) =
  print_endline "------------------------------------------------------";
  print_s [%message (game.round_num : int)];
  match Game_state.game_over game with
  | true -> end_processes game
  | false ->
    let _ =
      match Game_state.is_my_turn game with
      | true -> my_moves game
      | false -> opp_moves game
    in
    game.round_num <- game.round_num + 1;
    play_game ~game
    
;;
