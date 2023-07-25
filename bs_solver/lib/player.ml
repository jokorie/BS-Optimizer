open! Core
module T = struct
  type t =
  { (* maybe have two cases: Me & Opponent*)
    id : int (* represent position as well *)
  ; mutable hand_size : int
  ; mutable win_cycle : Card.t list
  ; mutable bluffs_completed : int
  (* ; mutable my_cards : int Card.Rank.Table.t *)
  (* ; mutable my_cards : (Card.Rank.t, int) Hashtbl.t *)
      (*remember to initialize all ranks as 0 in hashtbl*)
  }
[@@deriving sexp, fields]
end
include T
(* include Comparable.Make (T)
include Hashable.Make (T) *)







