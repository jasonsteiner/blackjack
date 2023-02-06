(** An engine for the evaluation of Blackjack game states

    This module represents an engine that evaluates a Blackjack game state and
    recommends the best move based on Blackjack strategy charts for 4 to 8-deck
    Blackjack games (dealer hits on Soft 17) as well as the optimal betting
    factor based on the HighLow card counting strategy. *)

val update_evaluation_idle : State.t -> State.t
(** [update_evaluation_idle st] updates the stored evaluation based on [st]. The
    evaluation is updated when no new cards have been played, i.e. only the deck
    size, number of decks, true count, best move, and bet size are updated. The
    result is [st], which is left unchanged. *)

val update_evaluation_curr_round : State.t -> State.t
(** [update_evaluation_curr_round st] updates the stored evaluation based on
    [st]. The evaluation is updated everytime after a card is drawn and the
    round is being played on. The result is [st], which is left unchanged. *)

val update_evaluation_new_round : State.t -> State.t
(** [update_evaluation_new_round st] updates the stored evaluation based on
    [st]. The evaluation is updated everytime after a new round is started. The
    result is [st], which is left unchanged. *)

val update_evaluation_dealer : State.t -> State.t
(** [update_evaluation_dealer st] updates the stored evaluation based on [st].
    The evaluation is updated everytime after the dealer hits the deck during
    their turn. The result is [st], which is left unchanged. *)

val update_evaluation_split : State.t -> State.t
(** [update_evaluation_split st] updates the stored evaluation based on [st].
    The evaluation is updated everytime after a split has occurred, and two
    additional cards have entered play. The result is [st], which is left
    unchanged. *)

val string_of_evaluation : unit -> string
(** [string_of_summary ()] is a string representation of the stored evaluation
    that is presentable to the player. *)
