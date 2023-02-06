(** CS 3110 Fall 2022 Final Project Blackjack System

    @author Andrew Liu (al849)
    @author Jason Steiner (jns226)
    @author Lincoln Stranger (las535) *)

(** An interactive single-player Blackjack system, simulating a Blackjack game
    with rules such as 4 to 8 playing decks, no "hole card" for the dealer,
    dealer hits until achieving a value of 17 or more, and a 2-1 Blackjack
    payout.

    The game supports traditional player decisions such as hitting, standing,
    doubling down, splitting, and surrendering as well as a betting mechanism.
    The user interface of the Blackjack game is fully text-based, and players
    can interact with the the interface by typing specific commands in the
    terminal. The game also offers an evaluation tool that suggests the
    recommended action to take and the recommended betting size, which is based
    on popular Blackjack strategy charts and a HighLow card counting system. *)
