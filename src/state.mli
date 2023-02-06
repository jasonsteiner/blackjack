(** Representation of dynamic Blackjack game

    This module represents the state of a Blackjack game as it is being played,
    including the current deck, the player's bets, available balance, and card
    hand, the dealer's hand, and functions that cause the state to change. *)

type h
(** The abstract type of values representing a Blackjack hand. *)

type t
(** The abstract type of values representing the Blackjack game state. *)

exception IllegalAction
(** Raised when an illegal action is taken in the Blackjack game state. *)

exception NegativeBet
(** Raised when a bet that is <=0 is placed in the Blackjack game state. *)

exception EmptyBalance
(** Raised when a bet is placed when the player has zero balance in the
    Blackjack game state. *)

val empty_hand : h
(** [empty_hand] is a hand with no cards. *)

val init_state : t
(** [init_state] is the primitive Blackjack game state with no current hands, an
    empty deck, zero balance, and no bets. *)

val add_deck : t -> Deck.t -> t
(** [add_deck st d] adds [d] to the top of the current deck in [st]. *)

val shuffle_deck : t -> t
(** [shuffle_deck st] shuffles the deck in [st]. *)

val load_state : t
(** [load_state] is the primitive Blackjack game state with a well-shuffled deck
    consisting of one to eight (inclusive) 52-card standard decks. *)

val deck_size : t -> int
(** [deck_size st] is the number of playing cards in the current deck in [st]. *)

val start_round : t -> t
(** [start_round st] is a new round of Blackjack with the player first receiving
    two cards from the top of the deck, then the dealer receiving one card from
    the top of the deck. If there are not enough cards in the deck to start the
    round, a standard 52-card deck is added to the existing deck (with
    shuffling) prior to distributing the cards to the hands. *)

val balance : t -> int
(** [balance st] is the current player balance. *)

val bet : t -> int -> t
(** [bet st n] is the resulting state after betting [n] on the current hand.
    Requires: [n] is non-negative. *)

val deposit : t -> int -> t
(** [deposit st n] is the resulting state after adding [n] to the current
    balance tracked by [st]. Requires: [n] is non-negative. *)

val current_bet : t -> int
(** [current_bet st] is the current bet on the current hand being played. *)

(** A type for representing the current turn. [Dealer] corresponds to the
    dealer's turn, [Player] corresponds to the player's turn playing their
    primary hand, and [PlayerSplit] corresponds to the player's turn playing
    their secondary hand resulting from the action of splitting. *)
type turn =
  | Dealer
  | Player
  | PlayerSplit

val current_turn : t -> turn
(** [current_turn] is the current turn of the Blackjack game. *)

val current_hand : t -> h
(** [current_hand st] is the current hand being played, either by the player or
    by the dealer. *)

val player_hands : t -> h * h
(** [player_hand st] is a pair of the player's hand(s). If the player has no
    second hand, the second hand by default is [empty_hand]. *)

val dealer_hand : t -> h
(** [dealer_hand st] is the dealer's hand. *)

val hit : t -> Card.t * t
(** [hit st] is the card drawn from choosing "hit" (i.e., taking another card)
    and the resulting state after completing the action. *)

val stand : t -> t
(** [stand st] is the resulting state after choosing "stand" for the current
    hand (i.e., take no more cards). The current hand swaps to the next hand to
    play. *)

val is_doubleable : h -> t -> bool
(** [is_doubleable h st] is whether or not the player can double the bet on [h]
    in [st]. *)

val double : t -> t
(** [double st] is the resulting state after choosing "double" for the current
    hand. The current hand takes exactly one more card, and the current bet on
    the hand is doubled. *)

val is_splittable : h -> t -> bool
(** [is_splittable h st] is whether or not the player can split the hand [h] in
    [st]. *)

val split : t -> t
(** [split st] is the resulting state after choosing "split" for the current
    hand. The starting hand with two cards is split into two separate hands,
    then each hand takes an extra card such that they both have two cards. The
    current hand switches to the newly created hand. Once a hand is split, the
    player can neither double nor split again, nor can they receive a Blackjack. *)

val is_surrenderable : h -> t -> bool
(** [is_surrenderable h st] is whether or not the player can surrender [h] in
    [st]. *)

val surrender : t -> t
(** [surrender st] is the resulting state after choosing "surrender" for the
    current hand. Half of the current bet, rounded up, is forfeited, and the
    current hand ends immediately. *)

val hand_size : h -> int
(** [hand_size h] is the number of cards in hand [h]. *)

val hand_contains_rank : Card.rank -> h -> bool
(** [hand_contains_rank r h] is whether or not hand [h] contains a card of rank
    [r]. *)

(** The type representing the best value of a Blackjack hand. *)
type value =
  | Blackjack
  | Value of int

val val_hand : h -> value
(** [val_hand h] is the current "best" value of hand [h]:

    - If [v] is the value of [h], then the result is [Value v]. If [h] has
      multiple possible values (i.e., [h] contains an Ace), [v] is the value
      maximum value less than or equal to 21, or if no such value exists, [v] is
      the minimum value.

    - If [h] is a Blackjack (i.e., [h] has size 2 and its value is exactly 21),
      then the result is [Blackjack]. *)

(** The type representing the current status of the Blackjack game. *)
type status =
  | DealerWin
  | SingleWin
  | MultiWin
  | Hand1L
  | Hand2L
  | BlackjackWin
  | Standoff
  | ContinueRound

val check_status : t -> status
(** [check_status st] is the current status of the Blackjack game. *)

val dealer_play : t -> t
(** [dealer_play st] is the resulting state after the dealer has drawn cards in
    [st] until the value of their hand is greater than or equal to 17. *)

val list_of_hand : h -> Card.t list
(** [list_of_hand h] is a list of cards in [h]. *)

val int_of_value : value -> int
(** [int_of_value v] is the integer equivalent value of value [v]. *)

val string_of_value : value -> string
(** [string_of_value v] is the string representation of value [v]. *)

val string_of_hand : h -> string
(** [string_of_hand h] is the string representation of hand [h]. *)

val firsthandloss : t -> t
(** [firsthandloss st] is the state resulting in tracking the loss of the first
    hand during split play. *)

val secondhandloss : t -> t
(** [secondhandloss st] is the state resulting in tracking the loss of the
    second hand during split play. *)
