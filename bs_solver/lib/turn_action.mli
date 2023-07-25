val conflicting_claim : game_state:Game_state.t -> claim:Card.Rank.t*int -> bool
val check_opponent_win : game_state:Game_state.t -> claim:Card.Rank.t*int -> bool
val useful_call : game_state:Game_state.t -> claim:Card.Rank.t*int -> bool
val card_probability : game_state:Game_state.t -> claim:Card.Rank.t*int -> bool
val assess_calling_bluff : game_state:Game_state.t -> claim:Card.Rank.t*int -> bool
val necessary_bluff : game_state:Game_state.t -> card:Card.Rank.t -> unit
val unnecessary_bluff : game_state:Game_state.t -> card:Card.Rank.t -> unit