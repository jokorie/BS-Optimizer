open! Core

module Suit : sig
  type t =
    | Heart
    | Diamomd
    | Spade
    | Club
  [@@deriving sexp, compare, hash]
end

module Rank : sig
  type t =
    | Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
end

type t =
| Known of
  { rank : Rank.t
  ; suit : Suit.t
  }
  |Unknown of {rank : Rank.t}
[@@deriving sexp, compare, hash]
