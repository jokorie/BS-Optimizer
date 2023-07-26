open! Core
open Util_functions

let conflicting_claim ~(game_state : Game_state.t) ~(claim : Card.t * int) =
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

let check_opponent_win ~(game_state : Game_state.t) ~(claim : Card.t * int) =
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

let useful_call ~(game_state : Game_state.t) ~(claim : Card.t * int) =
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
    let win_cycle = calc_win_cycle ~me:my_profile ~game_state in
    let immediate_win_cycle, _ = List.split_n win_cycle 5 in
    let cards_we_need =
      List.filter_map immediate_win_cycle ~f:(fun (rank, how_many) ->
        match how_many with 0 -> Some rank | _ -> None)
    in
    List.exists cards_we_need ~f:(fun card_needed ->
      Card.compare card_needed card_claimed = 0))
  else false
;;

let card_probability ~(game_state : Game_state.t) ~(claim : Card.t * int) =
  (*Relies on pure odds to assess whether or not we should call a bluff or
    not. Should return a bool! *)
  ignore game_state;
  ignore claim;
  true
;;

let assess_calling_bluff ~(game_state : Game_state.t) ~(claim : Card.t * int)
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

(* let rec fill_with_tail ~(win_cycle : (Card.t * int) list) ~(strategy :
   Strategy.t) : Strategy.t =

   match win_cycle with | [] -> strategy | _ -> ( let hd_win_cycle,
   _rest_win_cycle = List.split_n win_cycle 0 in let _card_needed, _how_many
   = hd_win_cycle in [] ) ;; *)

let rec lie_with_last_card
  ~(win_cycle : (Card.t * int) list)
  ~(strategy : Strategy.t)
  : Strategy.t
  =
  match win_cycle with
  | [] -> strategy
  | _ ->
    let hd_win_cycle, rest_win = List.split_n win_cycle 1 in
    let card, how_many = List.hd_exn hd_win_cycle in
    (match how_many with
     | 0 ->
       let rest_win_length = List.length rest_win in
       let beg_rest_win, tl_rest_win =
         List.split_n rest_win (rest_win_length - 1)
       in
       let last_card, qty = List.hd_exn tl_rest_win in
       (match qty with
        | 1 ->
          let strategy = strategy @ [ card, [ last_card ] ] in
          lie_with_last_card
            ~win_cycle:(chop_win_seq beg_rest_win)
            ~strategy
        | _ ->
          let strategy = strategy @ [ card, [ last_card ] ] in
          let new_win_cycle = beg_rest_win @ [ last_card, qty - 1 ] in
          lie_with_last_card
            ~win_cycle:(chop_win_seq new_win_cycle)
            ~strategy)
     | _ ->
       let cards_to_provide = List.init how_many ~f:(fun _ -> card) in
       let strategy = strategy @ [ card, cards_to_provide ] in
       lie_with_last_card ~win_cycle:(chop_win_seq rest_win) ~strategy)
;;

(* print_s[%message ((fill_missing_with_tail ~win_cycle:[]
   ~strategy:[]):Strategy.t)]; *)
(* List.fold win_cycle ~init:[] ~f:(fun strategy, (card, how_many) -> ( match
   how_many with | 0 -> ( let win_cycle_length = List.length win_cycle in let
   win_cycle_cut, tail_of_win_cycle = List.split_n (win_cycle_length -1) in
   let last_card, num = List.hd_exn tail_of_win_cycle in let bluff = [(card,
   [last_card])] in

   ) | _ -> )) *)

let necessary_bluff ~(game_state : Game_state.t) =
  (*In the event we are prompted to give a card we do not have, reccomend a
    bluff. Attempts to grab the furthest card we would need in our win cycle,
    unless only one of it is left, it is not optimal to lie on the last
    round.*)
  let me = Hashtbl.find_exn game_state.all_players game_state.my_id in
  let win_cycle = calc_win_cycle ~me ~game_state in
  let realized_win_cycle = chop_win_seq win_cycle in
  let last_needed_card, _ = List.last_exn realized_win_cycle in
  [ last_needed_card, 1 ]
;;

let unecessary_bluff ~(game_state : Game_state.t) ~(card : Card.t) =
  (*In the event we have the cards but want to overexaggerate how many cards
    we actually have, reccomend what to lie with*)
  ignore game_state;
  ignore card;
  []
;;

let my_turn_action ~(game_state : Game_state.t) ~(card : Card.t) =
  (*When it is our turn, assess if we have the cards we need to provide, and
    thus recommend a course of action.*)
  let me = Hashtbl.find_exn game_state.all_players game_state.my_id in
  let num_needed_card = Hashtbl.find_exn me.cards card in
  if num_needed_card > 0
  then unecessary_bluff ~game_state ~card
  else necessary_bluff ~game_state
;;
