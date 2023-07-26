open! Core

let conflicting_claim
  ~(game_state : Game_state.t)
  ~(claim : Card.Rank.t * int)
  =
  (*Assesses whether an opponent is lying based on the cards we have in our
    hand.*)
  let card, num_claimed = claim in
  let my_profile =
    Hashtbl.find_exn game_state.all_players game_state.my_id
  in
  let how_many_i_have = Hashtbl.find_exn my_profile.cards card in
  let available_cards = 4 - how_many_i_have in
  available_cards < num_claimed
;;

let check_opponent_win
  ~(game_state : Game_state.t)
  ~(claim : Card.Rank.t * int)
  =
  (*If an opponent's claim on their turn allows them to win the game, call
    bluff (GAME WOULD BE OVER IF THEY SUCCEEDED...) )*)
  let opponent_id = game_state.round_num % game_state.player_count in
  let opponent_profile =
    Hashtbl.find_exn game_state.all_players opponent_id
  in
  let opponent_hand_size = opponent_profile.hand_size in
  let _, num_claimed = claim in
  num_claimed - opponent_hand_size = 0
;;

let useful_call ~(game_state : Game_state.t) ~(claim : Card.Rank.t * int) =
  (*Assesses if calling a bluff would be incentivized regardlesss of the
    outcome, due to the claimed card being in the near future of our win
    cycle. ** Hardcoded threshold of "small pot" being 5 cards or less and
    "immediatley needed card" being within 4 win cycles.** *)
  let card_claimed, _ = claim in
  if List.length game_state.pot <= 5
  then (
    let my_profile =
      Hashtbl.find_exn game_state.all_players game_state.my_id
    in
    let win_cycle =
      Util_functions.calc_win_cycle ~me:my_profile ~game_state
    in
    let immediate_win_cycle, _ = List.split_n win_cycle 5 in
    let cards_we_need =
      List.filter_map immediate_win_cycle ~f:(fun (rank, how_many) ->
        match how_many with 0 -> Some rank | _ -> None)
    in
    List.exists cards_we_need ~f:(fun card_needed ->
      Card.Rank.compare card_needed card_claimed = 0))
  else false
;;

let card_probability
  ~(game_state : Game_state.t)
  ~(claim : Card.Rank.t * int)
  =
  (*Relies on pure odds to assess whether or not we should call a bluff or
    not.*)
  ignore game_state;
  ignore claim;
  true
;;

let assess_calling_bluff
  ~(game_state : Game_state.t)
  ~(claim : Card.Rank.t * int)
  =
  (*Runs through the different strategies to see if someone is bluffing or
    not. Strategy 1: If their claim conflicts woith the cards I currently
    have in my hand, call bluff. Strategy 2: If their claim causes them to
    win the game, call bluff. Strategy 3: If the pot is less than 5 and the
    card they are claiming is within the next 4 of my win cycle, call the
    bluff. *)
  if List.exists
       [ conflicting_claim ~game_state ~claim
       ; check_opponent_win ~game_state ~claim
       ; useful_call ~game_state ~claim
       ]
       ~f:(fun strategy_check -> strategy_check)
  then true
  else card_probability ~game_state ~claim
;;

let necessary_bluff ~(game_state : Game_state.t) ~(card : Card.Rank.t) =
  (*In the event we are prompted to give a card we do not have, reccomend a
    bluff.*)
  ignore game_state;
  ignore card
;;

let unecessary_bluff ~(game_state : Game_state.t) ~(card : Card.Rank.t) =
  (*In the event we have the cards but want to overexaggerate how many cards
    we actually have, reccomend what to lie with*)
  ignore game_state;
  ignore card
;;
