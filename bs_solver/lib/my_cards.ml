open! Core

type t = int Card.Rank.Table.t [@@deriving sexp]

(* include Hashable.Make (T) include
   Hashable.Make_plain_and_derive_hash_fold_t (T) *)
