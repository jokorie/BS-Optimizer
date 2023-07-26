open! Core

module Suit : sig
  type t =
    | Heart
    | Diamond
    | Spade
    | Club
  [@@deriving sexp, compare, hash]

  val of_char : char -> t
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
  [@@deriving sexp, compare, hash]

  val of_int : int -> t
  val of_char : char -> t

  include Hashable.S with type t := t
end

module Known_Card : sig
  type t =
    { rank : Rank.t
    ; suit : Suit.t
    }
end

module Unknown_Card : sig
  type t = { rank : Rank.t }
end

type t =
  | Known of Known_Card.t
  | Unknown of Unknown_Card.t
[@@deriving sexp, compare, hash]

val of_string : string -> t
