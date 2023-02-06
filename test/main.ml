(** Test file for Blackjack system. *)

(** ########################### TEST PLAN ###################################

    In evaluating the correctness of our blackjack system, we thoroughly tested
    files from which our game state is heavily reliant with OUnit testing,
    including those of card.ml, command.ml, deck.ml, and state.ml. In these
    files, no information is directly displayed to the user like in bin/main.ml,
    meaning that we can predictably test the outputs of the functions in the
    aforementioned files. To do so, we utilized various implementations of
    string conversion to test equality, as the data structures used for
    implementation were abstracted away. This made operations somewhat more
    difficult, as sometimes (like in state.ml testing), we could not compare
    outputs directly. In this example, since outputs often corresponded to a
    state of the game, we instead tested for certain properties that must hold
    true within the state after performing a specific operation. Not only did
    this help us guarantee the correctness of that individual function, but by
    incorporating other functions, we can guarantee their correctness and thus
    be confident that we do not need to test them further. An example of this
    would be string conversion, as we used it to check correctness of other
    implementations, and we would easily see (without testing the individual
    string function) that it is also correct.

    We utilized manual testing for the implementations of engine.ml and
    bin/main.ml. First, the implementation of engine is such that if it made an
    incorrect recommendation based on the state of the game, we could then take
    the provided recommendation in the game and see if it is incorrect. We were
    able to do this because we had pre-emptively tested the files containing
    actions, commands, decks, et cetera. Thus, if we were given an incorrect
    recommendation, the system would tell us that it's incorrect after we input
    the information into the text prompt. This is the reason why we put a
    significant amount of exception handlers in bin/main.ml in the case of the
    user inputting incorrect information. Additionally, as much of the engine is
    pattern matching card values with commands, it would be redundant to include
    test cases for them, as we can guarantee the correctness of those functions
    by way of the engine providing the corresponding recommendations in the text
    interface. Lastly, it was not necessary to test bin/main.ml because the
    outputs of each function will be displayed to the user throughout the game.
    Thus, if we made a mistake in that file, that mistake would immediately
    surface, and we would immediately notice it and isolate it in the file.

    Largely, our OUnit test cases (spanning card.ml, command.ml, deck.ml, and
    state.ml) were implemented using glass box testing. This is because,
    especially in state.ml, we can not test for equality with random decks and
    random drawing of cards. Thus, we had to create our own implementations of
    decks of cards, and we had to understand how, for example, the splitting
    mechanism worked in order to test for equality. In this case, we initalized
    a deck of four cards where the user would be dealt two splittable cards;
    then, we tested the resulting hands after splitting, converted to a string.
    This required multiple helper functions, as the data structure was
    abstracted away and we needed to test two entries in a tuple. Despite these
    techniques, there were a significant amount of functions that could be
    tested through black box testing, and it was largely in the more complex
    functions where we had to dig deeper into the particular implementation.

    We hope that the rationale above sufficiently explains our approach to
    testing. Through a mixture of automated OUnit testing and interaction with
    the text interface and command line, we were able to reliably guarantee
    correctness for our blackjack system. We believe it was a pragmatic
    approach, as if we had pursued automated testing for every aspect of the
    system, it would quickly become cumbersome and moreover a project in itself.
    By focusing on automated testing for the state-handling files and functions,
    we could effectively test and debug the rest of the system. *)

open OUnit2
open Blackjack
open Card
open Command
open Deck
open State

let test_card_1 = init_card Ace Diamonds [ 1; 11 ]
let test_card_2 = init_card (Number 2) Spades [ 2 ]
let test_card_3 = init_card (Number 3) Clubs [ 3 ]
let test_card_4 = init_card (Number 3) Diamonds [ 3 ]
let test_card_5 = init_card (Number 3) Spades [ 2 ]
let test_card_6 = init_card (Number 3) Diamonds [ 3; 4; 100; 13 ]
let test_card_7 = init_card Jack Diamonds [ 10 ]
let test_card_8 = init_card Queen Hearts [ 2 ]
let test_ace_of_spades = init_card Ace Spades [ 1; 11 ]
let test_add_on_empty = add empty test_card_1
let test_add_on_1card = add test_add_on_empty test_card_2
let test_another_1card_deck = add empty test_card_3
let test_combine_empty = combine empty test_another_1card_deck
let test_combine_nonempty = combine test_another_1card_deck test_add_on_1card

(* ########################### CARD TESTS ################################### *)

(** [equals_test name c1 c2 expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [Card.equals c1 c2]. *)
let equals_test (name : string) (c1 : Card.t) (c2 : Card.t)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (equals c1 c2) ~printer:string_of_bool

let equals_tests =
  [
    equals_test "Unequal rank, suit, and values" test_card_1 test_card_2 false;
    equals_test "Number rank (different int), suit, and values" test_card_2
      test_card_3 false;
    equals_test "Equal rank, unequal suit and values" test_card_3 test_card_5
      false;
    equals_test "Equal suit, unequal rank and values" test_card_1 test_card_7
      false;
    equals_test "Equal values, unequal rank and suit" test_card_2 test_card_8
      false;
    equals_test "Unequal values, equal rank and suit" test_card_4 test_card_6
      true;
    equals_test "Unequal rank, equal suit and values" test_card_5 test_card_2
      false;
    equals_test "Unequal suit, equal rank and values" test_card_3 test_card_4
      false;
    equals_test "Equal rank, suit, and values" test_ace_of_spades
      test_ace_of_spades true;
  ]

(** [string_of_int_list lst] is the string representation of list [lst]. *)
let rec string_of_int_list lst =
  "[" ^ String.concat "; " (List.map string_of_int lst) ^ "]"

(** [values_test name c expected_output] constructs an OUnit test named [name]
    that asserts the quality of [expected_output] with [Card.values c]. *)
let values_test (name : string) (c : Card.t) (expected_output : int list) : test
    =
  name >:: fun _ ->
  assert_equal expected_output (values c) ~printer:string_of_int_list

let values_tests =
  [
    values_test "2 of Spades" test_card_2 [ 2 ];
    values_test "Unofficial card" test_card_6 [ 3; 4; 100; 13 ];
    values_test "Ace of Diamonds" test_card_1 [ 1; 11 ];
  ]

(** [string_of_card_test name c expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [Card.string_of_card c]. *)
let string_of_card_test (name : string) (c : Card.t) (expected_output : string)
    : test =
  name >:: fun _ ->
  assert_equal expected_output (string_of_card c) ~printer:Fun.id

let string_of_card_tests =
  [
    string_of_card_test "Ace of Diamonds" test_card_1 "Ace of Diamonds";
    string_of_card_test "2 of Spades" test_card_2 "2 of Spades";
  ]

let card_tests =
  List.flatten [ equals_tests; values_tests; string_of_card_tests ]

(* ######################## COMMAND TESTS ################################### *)

(** [string_of_command] is the string representation of [c], a [Command.command]
    value. *)
let string_of_command (c : Command.command) : string =
  match c with
  | Hit -> "Hit"
  | Stand -> "Stand"
  | Quit -> "Quit"
  | Surrender -> "Surrender"
  | Double -> "Double"
  | Split -> "Split"
  | Evaluate -> "Evaluate"

(** [parse_test name str expected_output] constructs an OUnit test named [name]
    that asserts the quality of [expected_output] with [Command.parse str]. *)
let parse_test (name : string) (str : string)
    (expected_output : Command.command) : test =
  name >:: fun _ ->
  assert_equal expected_output (parse str) ~printer:string_of_command

(** [parse_fail_test name str e] constructs an OUnit test named [name] that
    asserts that exception [e] is appropriately raised by [Command.parse str].
    Invariant: [e] is either [Empty] or [Malformed]. *)
let parse_fail_test (name : string) (str : string) e : test =
  name >:: fun _ -> assert_raises e (fun () -> parse str)

let command_tests =
  [
    parse_test "Hit" "hit" Hit;
    parse_test "Stand" "    stand" Stand;
    parse_test "Quit" "    quit      " Quit;
    parse_test "Surrender" "surrender       " Surrender;
    parse_test "Double" " double " Double;
    parse_test "Split" "split" Split;
    parse_test "Evaluate" " evaluate" Evaluate;
    parse_fail_test "empty string" "" Empty;
    parse_fail_test "command with extra characters" "hit abc" Malformed;
    parse_fail_test "invalid command" "draw" Malformed;
  ]
(* ( " bet 500 is Bet 500" >:: fun _ -> assert_equal (Bet 500) (parse " bet 500
   ") ); ("hit is Hit" >:: fun _ -> assert_equal Hit (parse "hit")); *)

(* ########################### DECK TESTS ################################### *)

(** [string_of_deck_test name d expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [Deck.string_of_deck d]. *)
let string_of_deck_test (name : string) (d : Deck.t) (expected_output : string)
    : test =
  name >:: fun _ ->
  assert_equal expected_output (string_of_deck d) ~printer:Fun.id

(** [construct_ranks_string] is a string representation of cards with ranks
    [ranks] and suit [suit]. The order of the individual string representations
    is dependent on the order of the ranks specified in [ranks]. *)
let rec construct_ranks_string (ranks : string list) (suit : string) : string =
  match ranks with
  | [] -> ""
  | [ h ] -> h ^ " of " ^ suit
  | h :: t -> h ^ " of " ^ suit ^ ", " ^ construct_ranks_string t suit

(** [construct_standard_string] is a string representation of cards with ranks
    [ranks] and suits [suits]. The order of the individual string representation
    is dependent on the order of the ranks specified in [ranks] and the order of
    the suits specified in [suits]. The string representation is primarily
    sorted first by suit, then by rank (i.e. the suits are clustered together,
    then the ranks are ordered correctly). *)
let rec construct_standard_string (ranks : string list) (suits : string list) :
    string =
  match suits with
  | [] -> ""
  | [ h ] -> construct_ranks_string ranks h
  | h :: t ->
      construct_ranks_string ranks h ^ ", " ^ construct_standard_string ranks t

let standard_string =
  construct_standard_string
    [
      "Ace";
      "2";
      "3";
      "4";
      "5";
      "6";
      "7";
      "8";
      "9";
      "10";
      "Jack";
      "Queen";
      "King";
    ]
    [ "Spades"; "Hearts"; "Clubs"; "Diamonds" ]

let string_of_deck_tests =
  [
    string_of_deck_test "empty val test" empty "";
    string_of_deck_test "standard val test" standard standard_string;
    string_of_deck_test "card add test empty" test_add_on_empty
      "Ace of Diamonds";
    string_of_deck_test "card add test 1card already" test_add_on_1card
      "2 of Spades, Ace of Diamonds";
  ]

(** [size_test name d expected_output] constructs an OUnit test named [name]
    that asserts the quality of [expected_output] with [Deck.size d]. *)
let size_test (name : string) (d : Deck.t) (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (size d) ~printer:string_of_int

let size_tests =
  [
    size_test "size of empty is 0" empty 0;
    size_test "size of standard is 52" standard 52;
    size_test "size of one card deck" test_add_on_empty 1;
    size_test "size of two card deck" test_add_on_1card 2;
  ]

(** [combine_test] constructs an OUnit test named [name] that asserts the
    quality of [expected_output] with [Deck.combine d1 d2]. *)
let combine_test (name : string) (d1 : Deck.t) (d2 : Deck.t)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (string_of_deck (combine d1 d2)) ~printer:Fun.id

let combine_tests =
  [
    combine_test "combine two empty decks" empty empty "";
    combine_test "put nonempty deck on empty deck" empty standard
      standard_string;
    combine_test "put empty deck on nonempty deck" standard empty
      standard_string;
    combine_test "combine nonempty decks" standard standard
      (standard_string ^ ", " ^ standard_string);
  ]

(** [peek_test name d expected_output] constructs an OUnit test named [name]
    that asserts the quality of [expected_output] with [Deck.peek d]. *)
let peek_test (name : string) (d : Deck.t) (expected_output : Card.t) : test =
  name >:: fun _ ->
  assert_equal expected_output (peek d) ~printer:string_of_card

(** [peek_fail_test name d] constructs an OUnit test named [name] that asserts
    that exception [EmptyDeck] is raised by [Deck.peek d]. *)
let peek_fail_test (name : string) (d : Deck.t) : test =
  name >:: fun _ -> assert_raises EmptyDeck (fun () -> peek d)

let peek_tests =
  [
    peek_test "standard deck" standard test_ace_of_spades;
    peek_test "peeking 2 of spades" test_add_on_1card test_card_2;
    peek_fail_test "peeking empty deck" empty;
  ]

(** [add_test name d c expected_output] constructs an OUnit test named [name]
    that asserts the quality of [expected_output] with [Deck.add d c]. *)
let add_test (name : string) (d : Deck.t) (c : Card.t)
    (expected_output : Card.t) : test =
  name >:: fun _ ->
  assert_equal expected_output (peek (add d c)) ~printer:string_of_card

let add_tests =
  [
    add_test "add to empty deck" empty test_ace_of_spades test_ace_of_spades;
    add_test "add to standard deck" standard test_card_1 test_card_1;
    add_test "add to one card deck" test_add_on_empty test_card_2 test_card_2;
  ]

(** [draw_test name d expected_output] constructs an OUnit test named [name]
    that asserts the quality of [expected_output] with [Deck.draw d]. *)
let draw_test (name : string) (d : Deck.t) (expected_output : Card.t * Deck.t) :
    test =
  name >:: fun _ -> assert_equal expected_output (draw d)

(** [draw_test name d expected_output] constructs an OUnit test named [name]
    that asserts the quality of the equation specification of [Deck.draw d],
    namely:

    - If Deck.draw d = (c, d'), then Deck.add d' c = d. *)
let draw_eq_test (name : string) (d : Deck.t) : test =
  name >:: fun _ ->
  assert_equal d
    (let c, d' = draw d in
     add d' c)
    ~printer:string_of_deck

(** [draw_fail_test name d] constructs an OUnit test named [name] that asserts
    that exception [EmptyDeck] is raised by [Deck.draw d]. *)
let draw_fail_test (name : string) (d : Deck.t) : test =
  name >:: fun _ -> assert_raises EmptyDeck (fun () -> draw d)

let draw_tests =
  [
    draw_test "draw from one card deck" test_add_on_empty (test_card_1, empty);
    draw_test "draw from two card deck" test_add_on_1card
      (test_card_2, test_add_on_empty);
    draw_eq_test "standard deck equation specification" standard;
    draw_eq_test "1-card deck equation specification" test_another_1card_deck;
    draw_fail_test "drawing from empty deck" empty;
  ]

(** [shuffled_decks_generator] is a list of [n] randomly shuffled copies of deck
    [d]. Invariant: [n] > 0. *)
let shuffled_decks_generator n d =
  Array.make n d |> Array.to_list |> List.map shuffle

(** [shuffle_test name d n expected_output] constructs an OUnit test named
    [name] that asserts whether [d] is expected to appear in [n] randomly
    shuffled copies of [d]. This test leverages the fact that for a sufficiently
    large deck and for a sufficiently small [n], the probability of
    [Deck.shuffle d] being identical to [d] is extraordinarily unlikely:

    - For a standard 52-card deck, the probability of a shuffled copy of [d]
      having the same card ordering as [d] is on the order of O(n * 10 ^ -68).

    However, for deck sizes of 0 or 1, [Deck.shuffle d] is equal to [d].
    Invariant: [n] > 0. *)
let shuffle_test (name : string) (d : Deck.t) (n : int) (expected_output : bool)
    : test =
  let shuffled_decks = shuffled_decks_generator n d in
  name >:: fun _ ->
  assert_equal expected_output
    (List.mem d shuffled_decks)
    ~printer:string_of_bool

let shuffle_tests =
  [
    shuffle_test "empty deck" empty 1 true;
    shuffle_test "1-card deck" test_add_on_empty 1 true;
    shuffle_test "1 randomly shuffled standard deck" standard 1 false;
    shuffle_test "100 randomly shuffled standard decks" standard 100 false;
    shuffle_test "1000 randomly shuffled standard decks" standard 1000 false;
  ]

let deck_tests =
  List.flatten
    [
      string_of_deck_tests;
      size_tests;
      combine_tests;
      peek_tests;
      add_tests;
      draw_tests;
      shuffle_tests;
    ]

(* ########################## STATE TESTS ################################### *)

let standard_state = standard |> add_deck init_state
let single_card_state = test_add_on_empty |> add_deck standard_state
let double_standard_state = standard |> add_deck standard_state

(** [current_hand_test name st expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [State.hand_size (State.current_hand st)]. Since the cards in a hand are
    random, [State.current_hand st] is tested against the expected hand length. *)
let current_hand_test (name : string) (st : State.t) (expected_output : int) :
    test =
  name >:: fun _ ->
  assert_equal expected_output
    (hand_size (current_hand st))
    ~printer:string_of_int

let current_hand_tests =
  [ current_hand_test "initial primitive state" init_state 0 ]

(** [dealer_hand_test name st expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [State.hand_size (State.dealer_hand st)]. Since the cards in a hand are
    random, [State.dealer_hand st] is tested against the expected hand length. *)
let dealer_hand_test (name : string) (st : State.t) (expected_output : int) :
    test =
  name >:: fun _ ->
  assert_equal expected_output
    (hand_size (dealer_hand st))
    ~printer:string_of_int

let dealer_hand_tests =
  [ dealer_hand_test "initial primitive state" init_state 0 ]

(** [player_hands_test name st expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [State.player_hands st)]. Since the cards in a hand are random,
    [State.dealer_hand st] is tested against the expected hand length. *)
let player_hands_test (name : string) (st : State.t)
    (expected_output : int * int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (let h1, h2 = player_hands st in
     (hand_size h1, hand_size h2))

let player_hands_tests =
  [
    player_hands_test "initial primitive state" init_state (0, 0);
    player_hands_test "start round state" (init_state |> start_round) (2, 0);
  ]

(** [add_deck_test name st d expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [State.deck_size (State.add_deck st d)]. Since the order of the cards in a
    deck are random, [State.add_deck st d] is tested against the expected deck
    size. *)
let add_deck_test (name : string) (st : State.t) (d : Deck.t)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (deck_size (add_deck st d))
    ~printer:string_of_int

let add_deck_tests =
  [
    add_deck_test "add empty deck to initial primitive state" init_state empty 0;
    add_deck_test "add 1-card deck to initial primitive state" init_state
      test_add_on_empty 1;
    add_deck_test "add standard deck to initial primitive state" init_state
      standard 52;
    add_deck_test "add empty deck to state with nonempty deck" standard_state
      empty 52;
    add_deck_test "add nonempty deck to state with nonempty deck" standard_state
      standard 104;
  ]

(** [start_round_test name st expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with the deck size,
    current hand size, and dealer hand size (in this exact order) of
    [State.start_round st]. *)
let start_round_test (name : string) (st : State.t)
    (expected_output : int * int * int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (let st' = start_round st in
     ( deck_size st',
       st' |> current_hand |> hand_size,
       st' |> dealer_hand |> hand_size ))

let start_round_tests =
  [
    start_round_test "start round with initial primitive state" init_state
      (49, 2, 1);
    start_round_test "start round with 1-card deck state" single_card_state
      (50, 2, 1);
    start_round_test "start round with standard state" standard_state (49, 2, 1);
    start_round_test "start round with double standard state"
      double_standard_state (101, 2, 1);
  ]

let bet_state_1 = bet standard_state 200
let bet_state_2 = bet bet_state_1 300

(** [bet_test name st n expected_output] constructs an OUnit test named [name]
    that asserts the quality of [expected_output] with the balance of
    [State.bet st n]. *)
let bet_test (name : string) (st : State.t) (n : int) (expected_output : int) :
    test =
  name >:: fun _ -> assert_equal expected_output (bet st n |> balance)

let bet_tests =
  [
    bet_test "bet from standard_state" standard_state 200 300;
    bet_test "bet from bet_state_1" bet_state_1 300 0;
    ( "bet_state_2 is broke" >:: fun _ ->
      assert_raises EmptyBalance (fun () -> bet bet_state_2 1 |> balance) );
  ]

(** [deposit_test name st n expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with the balance of
    [State.deposit st n]. *)
let deposit_test (name : string) (st : State.t) (n : int)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (deposit st n |> balance)

let deposit_tests =
  [
    deposit_test "deposit from standard_state" standard_state 400 900;
    deposit_test "deposit from bet_state_1" bet_state_1 1000 1300;
    deposit_test "deposit from bet_state_2" bet_state_2 300 300;
  ]

let custom_deck_1 = init_card Ace Spades [ 1; 11 ] |> add empty
let custom_deck_2 = init_card (Number 2) Spades [ 2 ] |> add custom_deck_1
let custom_deck_3 = init_card (Number 3) Spades [ 3 ] |> add custom_deck_2
let custom_deck_4 = init_card (Number 4) Spades [ 4 ] |> add custom_deck_3
let custom_deck_5 = init_card (Number 5) Spades [ 5 ] |> add custom_deck_4
let custom_deck_6 = init_card (Number 6) Spades [ 6 ] |> add custom_deck_5
let custom_deck_7 = init_card (Number 7) Spades [ 7 ] |> add custom_deck_6
let custom_state = add_deck init_state custom_deck_7

(** [hit_test name st expected_output] constructs an OUnit test named [name]
    that asserts the quality of [expected_output] with the balance of
    [State.hit st]. *)
let hit_test (name : string) (st : State.t) (expected_output : Card.t) : test =
  name >:: fun _ -> assert_equal expected_output (hit st |> fst)

let hit_tests =
  [
    hit_test "bet from custom state" custom_state
      (init_card (Number 7) Spades [ 7 ]);
    hit_test "bet from second custom state"
      (add_deck init_state custom_deck_6)
      (init_card (Number 6) Spades [ 6 ]);
    hit_test "bet from custom state with only one card in deck"
      (add_deck init_state custom_deck_1)
      (init_card Ace Spades [ 1; 11 ]);
  ]

(** [current_hand_test st expected_output] constructs an OUnit test named [name]
    that asserts the quality of [expected_output] with the balance of
    [State.current_hand st]. *)
let current_hand_test (name : string) (st : State.t) (expected_output : State.h)
    : test =
  name >:: fun _ -> assert_equal expected_output (current_hand st)

let current_hand_tests =
  [
    current_hand_test "Player hand" init_state empty_hand;
    current_hand_test "Dealer hand" init_state empty_hand;
  ]

(** [is_doubleable_test h st expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with the balance of
    [State.is_doubleable h st]. *)
let is_doubleable_test (name : string) (h : State.h) (st : State.t)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (is_doubleable h st)

let doubleable_state_1 = custom_state |> hit |> snd |> hit |> snd |> hit |> snd
let doubleable_state_2 = custom_state |> hit |> snd |> hit |> snd

let is_doubleable_tests =
  [
    is_doubleable_test "too many cards"
      (player_hands doubleable_state_1 |> fst)
      doubleable_state_1 false;
    is_doubleable_test "not enough money"
      (bet doubleable_state_1 (balance doubleable_state_1)
      |> player_hands |> fst)
      doubleable_state_1 false;
    is_doubleable_test "two cards"
      (player_hands doubleable_state_2 |> fst)
      doubleable_state_2 true;
    is_doubleable_test "enough money"
      (bet doubleable_state_2 200 |> player_hands |> fst)
      doubleable_state_2 true;
  ]

(** [double_test st expected_output] constructs an OUnit test named [name] that
    asserts the quality of [expected_output] with the balance of
    [State.double st]. *)
let double_test (name : string) (st : State.t) (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (double st |> balance)

let double_tests =
  [
    double_test "doubling bet of 200 leaves 100 left from inital balance"
      (bet doubleable_state_2 200)
      100;
  ]

let split_card_1 = init_card (Number 4) Spades [ 4 ]
let split_card_2 = init_card (Number 7) Spades [ 7 ]
let split_card_3 = init_card (Number 2) Spades [ 2 ]
let split_card_4 = init_card (Number 2) Hearts [ 2 ]
let splittable_deck = split_card_1 |> add empty
let splittable_deck' = split_card_2 |> add splittable_deck
let splittable_deck'' = split_card_3 |> add splittable_deck'
let splittable_deck''' = split_card_4 |> add splittable_deck''

let splittable_state =
  add_deck init_state splittable_deck''' |> hit |> snd |> hit |> snd

let splittable_state' = add_deck init_state splittable_deck''' |> hit |> snd

(** [is_splittable_test st expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with the balance of
    [State.is_splittable st]. *)
let is_splittable_test (name : string) (h : State.h) (st : State.t)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (is_splittable h st)

let is_splittable_tests =
  [
    is_splittable_test "rank matches"
      (bet splittable_state 200 |> player_hands |> fst)
      splittable_state true;
    is_splittable_test "impossible rank matching"
      (bet splittable_state' 200 |> player_hands |> fst)
      splittable_state' false;
  ]

(** [split_test_fst st expected_output] constructs an OUnit test named [name]
    that asserts the quality of [expected_output] with the balance of
    [State.split st]. *)
let split_test_fst (name : string) (st : State.t) (expected_output : string) :
    test =
  name >:: fun _ ->
  assert_equal expected_output
    (split st |> player_hands |> fst |> string_of_hand)
    ~printer:Fun.id

(** [split_test_snd st expected_output] constructs an OUnit test named [name]
    that asserts the quality of [expected_output] with the balance of
    [State.split st]. *)
let split_test_snd (name : string) (st : State.t) (expected_output : string) :
    test =
  name >:: fun _ ->
  assert_equal expected_output
    (split st |> player_hands |> snd |> string_of_hand)
    ~printer:Fun.id

let split_tests =
  [
    split_test_fst "first hand resulting from split" splittable_state
      "2 of Hearts, 7 of Spades";
    split_test_snd "second hand resulting from split" splittable_state
      "2 of Spades, 4 of Spades";
  ]

(** [is_surrenderable_test st expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with the balance of
    [State.is_surrenderable st]. *)
let is_surrenderable_test (name : string) (h : State.h) (st : State.t)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (is_surrenderable h st)

let is_surrenderable_tests =
  [
    is_surrenderable_test "hand size is two"
      (bet splittable_state 200 |> player_hands |> fst)
      splittable_state true;
    is_surrenderable_test "hand size is not two"
      (bet splittable_state' 200 |> player_hands |> fst)
      splittable_state' false;
  ]

(** [surrender_test st expected_output] constructs an OUnit test named [name]
    that asserts the quality of [expected_output] with the balance of
    [State.double st]. *)
let surrender_test (name : string) (st : State.t) (expected_output : int) : test
    =
  name >:: fun _ ->
  assert_equal expected_output (surrender st |> balance) ~printer:string_of_int

let surrender_tests =
  [
    surrender_test "bet 200 from initial 500, surrender returns 100"
      (bet splittable_state 200) 400;
    surrender_test "bet 200 from initial 500, surrender returns 100"
      (bet splittable_state 500) 250;
  ]

(** [val_hand_test name st d expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [State.val_hand (State.current_hand st)]. *)
let val_hand_test (name : string) (st : State.t) (expected_output : State.value)
    : test =
  name >:: fun _ -> assert_equal expected_output (val_hand (current_hand st))

let val_hand_tests =
  [ val_hand_test "initial primitive state" init_state (Value 0) ]

let state_tests =
  List.flatten
    [
      current_hand_tests;
      dealer_hand_tests;
      player_hands_tests;
      add_deck_tests;
      start_round_tests;
      bet_tests;
      deposit_tests;
      hit_tests;
      current_hand_tests;
      is_doubleable_tests;
      double_tests;
      is_splittable_tests;
      split_tests;
      is_surrenderable_tests;
      surrender_tests;
      val_hand_tests;
    ]

(* ########################## TEST SUITE #################################### *)

let suite =
  "test suite for final project"
  >::: List.flatten [ card_tests; command_tests; deck_tests; state_tests ]

let _ = run_test_tt_main suite
