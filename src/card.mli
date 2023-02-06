(** Representation of a playing card.

    This module represents a single playing card, including the rank, suit, and
    possible values of the card. *)

type t
(** The abstract type of values representing a singular playing card. *)

(** The type [rank] represents the possible rank of the playing card. These
    ranks are the twelve standard playing card ranks. Invariant: the [int]
    carried by [Number] is an integer in 2..10. *)
type rank =
  | Ace
  | Number of int
  | Jack
  | Queen
  | King

(** The type [suit] represents the possible suits of the playing card. These
    suits are the four standard playing card suits. *)
type suit =
  | Diamonds
  | Hearts
  | Spades
  | Clubs

val init_card : rank -> suit -> int list -> t
(** [init_card r s v] is a playing card with rank [r], suit [s], and values [v]. *)

val rank : t -> rank
(** [rank c] is the rank of the playing card [c]. *)

val suit : t -> suit
(** [suit c] is the suit of the playing card [c]. *)

val values : t -> int list
(** [values c] is a list of values associated with the playing card [c]. *)

val is_rank : t -> rank -> bool
(** [is_rank c r] is whether or not card [c] has rank [r]. *)

val equals : t -> t -> bool
(** [equals c1 c2] is whether or not both the ranks and suits of [c1] and [c2]
    are the same. *)

val string_of_card : t -> string
(** [string_of_card c] is a string representation of the playing card [c]. *)