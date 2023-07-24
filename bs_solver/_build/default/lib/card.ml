open! Core

module Suit = struct
  module T = struct
    type t = 
  | Heart | Diamomd | Spade | Club
  [@@deriving sexp, compare, hash]
  end
  
include T
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
end



    type t = 
    {
    rank: Rank.t;
    suit:Suit.t;
    }
  

  [@@deriving sexp, compare, hash]
