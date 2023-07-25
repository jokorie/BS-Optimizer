open! Core

type t = int Card.Rank.Table.t [@@deriving sexp]

val init : unit -> t
val add_card : t -> card:Card.t -> unit
