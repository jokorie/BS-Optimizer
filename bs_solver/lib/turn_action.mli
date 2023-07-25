val conflicting_claim : game_state:'a -> claim:'b * int -> bool
val check_opponent_win : game_state:'a -> claim:'b * int -> bool
val useful_call : game_state:'a -> claim:'b * int -> bool
val card_probability : game_state:'a -> claim:'b * int -> bool
val assess_calling_bluff : game_state:'a -> claim:'b * int -> bool
