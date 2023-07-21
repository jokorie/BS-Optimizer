open! Core

module Suit = struct
  module T = struct
    type t =
      | Heart
      | Diamomd
      | Spade
      | Club
    [@@deriving sexp, compare, hash]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

module Rank = struct
  module T = struct
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
  end
  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

type t =
  | Known of
      { rank : Rank.t
      ; suit : Suit.t
      }
  | Unknown of { rank : Rank.t }
[@@deriving sexp, compare, hash]
