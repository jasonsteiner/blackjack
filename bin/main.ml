open Blackjack.Card
open Blackjack.Command
open Blackjack.Deck
open Blackjack.Engine
open Blackjack.State

(** [count] is the number of rounds that have been played. *)
let count = ref 0

(** [wins] is the number of wins the player has. *)
let wins = ref 0

(** [losses] is the number of losses the player has. *)
let losses = ref 0

(** [quit_prompt ()] prints a farewell message, then terminates the Blackjack
    game without error messages and exceptions. *)
let quit_prompt () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\
     You have successfully quit the Blackjack game. Thank you for playing! \
     Exiting the session...\n";
  Stdlib.exit 0

let emptybal_prompt () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\
     Oh no! You have run out of money! Thank you for playing! Exiting the \
     session...\n";
  Stdlib.exit 0

(** [print_hands st] prints a prompt containing information of the cards in the
    dealer's hand and player's hand. *)
let print_hands st =
  let p1, p2 = player_hands st in
  let d_hand = dealer_hand st in
  ANSITerminal.print_string [ ANSITerminal.yellow ] "The Dealer's hand is: ";
  let dealer_value =
    if val_hand d_hand = Blackjack then Value 21 else val_hand d_hand
  in
  print_endline
    (string_of_hand d_hand ^ "  (Current Value: "
    ^ string_of_value dealer_value
    ^ ")");
  match p2 = empty_hand with
  | true ->
      ANSITerminal.print_string [ ANSITerminal.yellow ] "Your hand is: ";
      print_endline
        (string_of_hand p1 ^ "  (Current Value: "
        ^ string_of_value (val_hand p1)
        ^ ")")
  | false ->
      ANSITerminal.print_string [ ANSITerminal.yellow ] "Your hands are:\n";
      print_endline
        ("[Primary Hand] " ^ string_of_hand p1 ^ "  (Current Value: "
        ^ string_of_value (val_hand p1)
        ^ ")");
      print_endline
        ("[Split Hand] " ^ string_of_hand p2 ^ "  (Current Value: "
        ^ string_of_value (val_hand p2)
        ^ ")")

(** [busted_prompt ()] prints a prompt for when the player busts their hand. *)
let busted_prompt () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\nYou went bust!\n";
  incr losses;
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    "\n\
     ################################################################################################\n\n";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "Starting a new round...\n\n"

(** [blackjack_prompt ()] prints a prompt for when the player hits a Blackjack.*)
let blackjack_prompt st =
  ANSITerminal.print_string [ ANSITerminal.green ] "\nYou hit a Blackjack!\n";
  incr wins;
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    "\n\
     ################################################################################################\n\n";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "Starting a new round...\n\n";
  let st' = deposit st (current_bet st * 3) in
  st'

let rec bet_prompt st =
  print_string "\nYour current balance is: ";
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("$" ^ string_of_int (balance st) ^ ".\n");
  if balance st <= 0 then emptybal_prompt ()
  else print_endline "\nHow much would you like to bet? Enter an integer.";
  print_string "> ";
  match int_of_string (read_line ()) with
  | x -> (
      try
        ANSITerminal.print_string [ ANSITerminal.red ]
          ("\nYou have $"
          ^ string_of_int (bet st x |> balance)
          ^ " remaining.\n\n");
        let st' = bet st x in
        st'
      with
      | IllegalAction ->
          let () =
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\nYou cannot bet more than you have.\n"
          in
          bet_prompt st
      | NegativeBet ->
          let () =
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\nYou must bet more than 0.\n"
          in
          bet_prompt st
      | EmptyBalance -> emptybal_prompt ())
  | exception _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\nYou have entered an invalid value. Please try again.\n";
      bet_prompt st

(** [double_prompt st] prints a prompt for when the player doubles their bet. *)
let double_prompt st =
  try
    let st' = double st in
    ANSITerminal.print_string [ ANSITerminal.green ]
      "\n\nDoubling your bet...\n";
    print_string "\nYour total bet is now: ";
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ("$" ^ string_of_int (st' |> current_bet) ^ "\n");
    ANSITerminal.print_string [ ANSITerminal.red ]
      ("\nYou have $" ^ string_of_int (st' |> balance) ^ " remaining.\n\n");
    print_hands st;
    st'
  with
  | IllegalAction ->
      let () =
        ANSITerminal.print_string [ ANSITerminal.red ]
          "\n\
           Unable to double. You must have enough money, and you cannot have \
           already taken an action.\n\n"
      in
      print_hands st;
      st
  | NegativeBet ->
      let () =
        ANSITerminal.print_string [ ANSITerminal.red ]
          "\nYou must bet more than $0.\n\n"
      in
      print_hands st;
      st
  | EmptyBalance ->
      let () =
        ANSITerminal.print_string [ ANSITerminal.red ]
          "\nYou cannot double with an empty balance.\n\n"
      in
      print_hands st;
      st

(** [new_round_prompt st] is the new state resulting from starting a new round.
    It also handles start the round and provides information about the new
    round, i.e. dealing new cards to a fresh hand to the player and dealer, and
    informing the player of their hand, the dealer's hand, and the values. *)
let rec new_round_prompt st =
  incr count;
  ANSITerminal.print_string [ ANSITerminal.magenta ] "\nDealing new cards for ";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    ("round " ^ string_of_int !count ^ "\n");
  ANSITerminal.print_string [ ANSITerminal.green ]
    ("Wins: " ^ string_of_int !wins ^ "\n");
  ANSITerminal.print_string [ ANSITerminal.red ]
    ("Losses: " ^ string_of_int !losses ^ "\n");
  let st' = start_round st |> update_evaluation_new_round in
  let st' = bet_prompt st' in
  print_string "Current Bet: ";
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("$" ^ string_of_int (current_bet st') ^ "\n\n");
  print_hands st';
  if check_status st' = BlackjackWin then
    let st' = blackjack_prompt st' in
    new_round_prompt st'
  else st'

(** [hit_prompt st] is the new state resulting from the player playing the "hit"
    action. It also handles printing relevant information for the action. *)
let hit_prompt st =
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    "\n\nHitting the deck...\n\n";
  let c, st' = hit st in
  ANSITerminal.print_string [ ANSITerminal.yellow ] "You have drawn: ";
  print_endline (string_of_card c);
  print_hands st';
  st'

(** [split_prompt st] is the new state resulting from the player playing the
    "split" action. It also handles printing relevant information for the
    action. *)
let split_prompt st =
  try
    let st' = split st |> update_evaluation_split in
    ANSITerminal.print_string [ ANSITerminal.magenta ]
      "\nSplitting the deck...\n";
    print_string "\nYour total bet is now: ";
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ("$" ^ string_of_int (st' |> current_bet));
    print_string " (half on each hand)";
    print_string "\nYour current balance is: ";
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ("$" ^ string_of_int (balance st) ^ ".\n\n");
    print_hands st';
    st'
  with IllegalAction ->
    ANSITerminal.print_string [ ANSITerminal.red ]
      "\n\
       Unable to split. You must have a hand with two cards of the same rank \
       from the initial deal, and you must have enough money.\n\n";
    print_hands st;
    st

(** [surrender_prompt st] prints a prompt for when the player surrenders their
    hand. *)
let surrender_prompt st =
  try
    let st' = surrender st in
    ANSITerminal.print_string [ ANSITerminal.red ] "\nYou have surrendered!\n";
    ANSITerminal.print_string [ ANSITerminal.magenta ]
      "\n\
       ################################################################################################\n\n";
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      "Starting a new round...\n";
    incr losses;
    st' |> new_round_prompt
  with IllegalAction ->
    ANSITerminal.print_string [ ANSITerminal.red ]
      "\n\n\
       Unable to surrender. You have already taken an action for your current \
       hand.\n\n";
    print_hands st;
    st

(** [dealer_end_prompt st] prints the prompt corresponding to the current state
    of the Blackjack game after the dealer has finished playing their turn. *)
let dealer_end_prompt st =
  match check_status st with
  | SingleWin ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "\nYou won! You were better than the Dealer!\nPaying out bet...\n";
      incr wins;
      ANSITerminal.print_string [ ANSITerminal.magenta ]
        "\n\
         ################################################################################################\n\n";
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "Starting a new round...\n";
      let st' = deposit st (current_bet st * 2) in
      st'
  | DealerWin ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\nYou lost. The Dealer's hand was better than yours.\n";
      incr losses;
      ANSITerminal.print_string [ ANSITerminal.magenta ]
        "\n\
         ################################################################################################\n\n";
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "Starting a new round...\n";
      st
  | MultiWin ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "\nYou won big! Both of your hands won!\n";
      incr wins;
      ANSITerminal.print_string [ ANSITerminal.magenta ]
        "\n\
         ################################################################################################\n\n";
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "Starting a new round...\n";
      let st' = deposit st (current_bet st * 2) in
      st'
  | _ -> raise (Failure "Unnecessary pattern match")

(** [dealer_prompt st] is the new state resulting from the dealer playing. It
    also handles printing relevant information for the dealer's turn. *)
let dealer_prompt st =
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    "The Dealer is now playing...\n\n";
  let st' = dealer_play st |> update_evaluation_dealer in
  print_hands st';
  let st' = dealer_end_prompt st' in
  new_round_prompt st'

(** [hand1l_prompt st] is the new state resulting from the player going bust on
    their first hand during a split play. It also handles printing relevant
    information for the action. *)
let hand1l_prompt st =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\nYour first hand was a bust!\n";
  let st' = firsthandloss st in
  print_string "\nYour total bet is now: ";
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("$" ^ string_of_int (st' |> current_bet) ^ "\n\n");
  let st' = stand st' in
  let () =
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      "Now playing your second hand...\n"
  in
  print_hands st';
  st' |> update_evaluation_idle

(** [hand2l_prompt st] is the new state resulting from the player going bust on
    their second hand during a split play. It also handles printing relevant
    information for the action. *)
let hand2l_prompt st =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\nYour second hand was a bust!\n";
  let st' = secondhandloss st in
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    ("\nYour bet is now $"
    ^ string_of_int (st' |> current_bet)
    ^ ", remaining on your first hand.\n\n");
  let st' = stand st' in
  dealer_prompt st'

(** [stand_prompt st] is the new state resulting from the player playing the
    "stand" action. It also handles printing relevant information for the
    action. *)
let stand_prompt st =
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    "\nStanding for your current hand...\n";
  let st' = stand st in
  if current_turn st' = Dealer then dealer_prompt st'
  else
    let () =
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "Now playing your second hand...\n\n"
    in
    print_hands st';
    st' |> update_evaluation_idle

(** [main_prompt st] handles the player's inputs and prints the appropriate
    prompts corresponding to the parsed inputs. *)
let rec main_prompt st =
  print_string "\nValid commands are: ";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "| hit | stand | split | double | surrender | evaluate | quit |\n";
  print_endline "What would you like to do?";
  print_string "> ";
  match read_line () with
  | input -> begin
      match parse input with
      | exception Empty ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            "\nYou have not entered an action. Please try again.\n";
          main_prompt st
      | exception Malformed ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            "\nYou have entered an invalid action. Please try again.\n";
          main_prompt st
      | Quit -> quit_prompt ()
      | Hit -> (
          let st' = st |> hit_prompt |> update_evaluation_curr_round in
          match check_status st' with
          | Hand1L -> st' |> hand1l_prompt |> main_prompt
          | Hand2L -> st' |> hand2l_prompt |> main_prompt
          | DealerWin ->
              let () = busted_prompt () in
              new_round_prompt st' |> main_prompt
          | BlackjackWin ->
              let st' = blackjack_prompt st in
              new_round_prompt st' |> main_prompt
          | ContinueRound -> main_prompt st'
          | _ -> raise (Failure "Unnecessary pattern match"))
      | Stand -> stand_prompt st |> main_prompt
      | Split -> split_prompt st |> main_prompt
      | Evaluate ->
          let _ = update_evaluation_idle st in
          ANSITerminal.print_string [ ANSITerminal.yellow ]
            (string_of_evaluation () ^ "\n");
          main_prompt st
      | Surrender -> surrender_prompt st |> main_prompt
      | Double -> (
          match double st with
          | exception _ -> st |> double_prompt |> main_prompt
          | _ -> (
              let st' =
                st |> double_prompt |> hit_prompt
                |> update_evaluation_curr_round
              in
              match check_status st' with
              | DealerWin ->
                  let () = busted_prompt () in
                  new_round_prompt st' |> main_prompt
              | BlackjackWin ->
                  let st' = blackjack_prompt st in
                  new_round_prompt st' |> main_prompt
              | ContinueRound -> st' |> stand_prompt |> main_prompt
              | _ -> raise (Failure "Unnecessary pattern match")))
    end

(** [main ()] starts a session of the Blackjack game. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "\n\nWelcome to the Text-based Single-player Blackjack Game.\n\n";
  ANSITerminal.print_string [ ANSITerminal.green ]
    "Initializing new Blackjack session...\n";
  let session = load_state in
  let num_decks = deck_size session / 52 in
  ANSITerminal.print_string [ ANSITerminal.green ]
    ("Successfully loaded new Blackjack game with " ^ string_of_int num_decks
   ^ " 52-card standard decks...\n\n");
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "Exceptional Blackjack Rules\n";
  print_endline
    "For the basic rules of Blackjack, you can find many helpful resources \
     online. \n\
     However, here are several rules that may differ from traditional \
     Blackjack game rules or are particularly noteworthy:";
  print_endline
    "1) The dealer hits and continues to hit the deck until they achieve a \
     value of 17 or above, \n\
     even if they have already beaten your hand value.";
  print_endline "2) The dealer has no \"hole card\".";
  print_endline "3) Blackjacks pay out at 2x your original bet.";
  print_endline
    "4) Surrendering pays back half your original bet, rounded down to the \
     nearest integer.";
  print_endline
    "5) Once the deck runs out of cards, one standard 52-card deck is shuffled \
     and added to the game. \n\
     You will receive no visual prompt of this occuring unless you use the \
     evaluation.";
  let session' = new_round_prompt session in
  main_prompt session'

(* Execute the Blackjack game session. *)
let () = main ()