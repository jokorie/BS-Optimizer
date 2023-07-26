open! Core

module Suit = struct
  module T = struct
    type t =
      | Heart
      | Diamond
      | Spade
      | Club
    [@@deriving sexp, compare, hash]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let of_char char =
    match Char.lowercase char with
    | 'h' -> Heart
    | 'd' -> Diamond
    | 's' -> Spade
    | 'c' -> Club
    | _ -> failwith "invalid char"
  ;;

  (* include Hashable.Make_plain_and_derive_hash_fold_t (T) *)
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

  (* include Hashable.Make_plain_and_derive_hash_fold_t (T) *)
  let of_int int =
    match int with
    | 1 -> Ace
    | 2 -> Two
    | 3 -> Three
    | 4 -> Four
    | 5 -> Five
    | 6 -> Six
    | 7 -> Seven
    | 8 -> Eight
    | 9 -> Nine
    | 10 -> Ten
    | 11 -> Jack
    | 12 -> Queen
    | 13 -> King
    | _ -> failwith "invalid int"
  ;;

  let of_char char =
    match Char.uppercase char with
    | 'A' -> Ace
    | '2' -> Two
    | '3' -> Three
    | '4' -> Four
    | '5' -> Five
    | '6' -> Six
    | '7' -> Seven
    | '8' -> Eight
    | '9' -> Nine
    | 'T' -> Ten
    | 'J' -> Jack
    | 'Q' -> Queen
    | 'K' -> King
    | _ -> failwith "invalid char"
  ;;
end

module Known_Card = struct
  type t =
    { rank : Rank.t
    ; suit : Suit.t
    }
  [@@deriving sexp, compare, hash, fields]
end

module Unknown_Card = struct
  type t = { rank : Rank.t } [@@deriving sexp, compare, hash, fields]
end

type t =
  | Known of Known_Card.t
  | Unknown of Unknown_Card.t
[@@deriving sexp, compare, hash]

let of_string string =
  let rank = Rank.of_char (String.get string 0) in
  let suit = Suit.of_char (String.get string 1) in
  Known { rank; suit }
;;
