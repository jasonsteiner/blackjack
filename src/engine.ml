open Card
open State

type move =
  | Hit
  | Stand
  | Double
  | Split
  | Surrender

type t = {
  mutable best_move : move;
  mutable bet_size : float;
  mutable deck_size : int;
  mutable card_count : int;
  mutable num_decks : float;
  mutable true_count : float;
}

(** [hard_chart h dc st] is [move] recommended by the 4-8 Decks, Dealer hits on
    Soft 17 Blackjack strategy chart (hard values) given player hand [h], dealer
    card [dc], and game state [st]. The implemented chart is sourced from
    https://wizardofodds.com/games/blackjack/strategy/4-decks/. *)
let hard_chart h dc st =
  let v = int_of_value (val_hand h) in
  let dv = int_of_value (val_hand dc) in
  let can_double = is_doubleable h st in
  let can_surrender = is_surrenderable h st in
  match v with
  | 4 | 5 | 6 | 7 | 8 -> Hit
  | 9 -> if dv >= 3 && dv <= 6 && can_double then Double else Hit
  | 10 -> if dv <= 9 && can_double then Double else Hit
  | 11 -> if can_double then Double else Hit
  | 12 -> if dv >= 4 && dv <= 6 then Stand else Hit
  | 13 | 14 -> if dv <= 6 then Stand else Hit
  | 15 ->
      if dv <= 6 then Stand
      else if dv >= 10 && can_surrender then Surrender
      else Hit
  | 16 ->
      if dv <= 6 then Stand
      else if dv >= 9 && can_surrender then Surrender
      else Hit
  | 17 -> if dv >= 11 && can_surrender then Surrender else Stand
  | _ -> Stand

(** [soft_chart h dc st] is [move] recommended by the 4-8 Decks, Dealer hits on
    Soft 17 Blackjack strategy chart (soft values) given player hand [h], dealer
    card [dc], and game state [st]. The implemented chart is sourced from
    https://wizardofodds.com/games/blackjack/strategy/4-decks/. *)
let soft_chart h dc st =
  let v = int_of_value (val_hand h) in
  let dv = int_of_value (val_hand dc) in
  let can_double = is_doubleable h st in
  match v with
  | 13 | 14 -> if (dv = 5 || dv = 6) && can_double then Double else Hit
  | 15 | 16 -> if dv >= 4 && dv <= 6 && can_double then Double else Hit
  | 17 -> if dv >= 3 && dv <= 6 && can_double then Double else Hit
  | 18 ->
      if dv >= 9 then Hit else if dv <= 6 && can_double then Double else Stand
  | 19 -> if dv = 6 && can_double then Double else Stand
  | _ -> Stand

(** [split_chart h dc st] is [move] recommended by the 4-8 Decks, Dealer hits on
    Soft 17 Blackjack strategy chart (splitting) given player hand [h], dealer
    card [dc], and game state [st]. The implemented chart is sourced from
    https://wizardofodds.com/games/blackjack/strategy/4-decks/. *)
let split_chart h dc st =
  let v = int_of_value (val_hand h) in
  let dv = int_of_value (val_hand dc) in
  let can_double = is_doubleable h st in
  match v with
  | 4 | 6 -> if dv >= 8 then Hit else Split
  | 8 -> Hit
  | 10 -> if dv <= 9 && can_double then Double else Hit
  | 12 ->
      if hand_contains_rank Ace h then Split
      else if dv >= 3 && dv <= 6 then Split
      else Hit
  | 14 -> if dv <= 7 then Split else Hit
  | 16 -> if dv <= 10 then Split else Surrender
  | 18 -> if dv = 7 || dv = 10 || dv = 11 then Stand else Split
  | 20 -> Stand
  | _ -> failwith "Impossible hand value"

(** [get_best_move st] is the best move for the player to make in [st] using the
    recommended moves in Blackjack strategy charts. *)
let get_best_move st =
  let h = current_hand st in
  let dc = dealer_hand st in
  if is_splittable h st then split_chart h dc st
  else if hand_contains_rank Ace h then soft_chart h dc st
  else hard_chart h dc st

(** [evaluation] is the evaluation of the game state. Before any rounds have
    occurred, it is initialized with information that may or may not be
    accurate. *)
let evaluation =
  {
    best_move = Hit;
    bet_size = 0.;
    deck_size = 0;
    card_count = 0;
    num_decks = 0.;
    true_count = 0.;
  }

(** [new_card_count c] is the new card count given that card [c] was most
    recently played with current card count [evaluation.card_count] using the
    HighLow Count System.*)
let new_card_count c =
  if
    is_rank c Ace || is_rank c Jack || is_rank c Queen || is_rank c King
    || is_rank c (Number 10)
  then evaluation.card_count - 1
  else if
    is_rank c (Number 2) || is_rank c (Number 3) || is_rank c (Number 4)
    || is_rank c (Number 5) || is_rank c (Number 6)
  then evaluation.card_count + 1
  else evaluation.card_count

let update_evaluation_idle st =
  let deck_size = deck_size st in
  evaluation.deck_size <- deck_size;
  evaluation.num_decks <- float_of_int deck_size /. 52.;
  evaluation.true_count <-
    float_of_int evaluation.card_count /. evaluation.num_decks;
  if current_turn st <> Dealer then
    let () = evaluation.best_move <- get_best_move st in
    let () =
      evaluation.bet_size <-
        (let tc = evaluation.true_count -. 1. in
         if tc <= 0. then 1. else tc)
    in
    st
  else st

let update_evaluation_curr_round st =
  let most_recent_card =
    st |> current_hand |> list_of_hand |> List.rev |> List.hd
  in
  evaluation.card_count <- new_card_count most_recent_card;
  update_evaluation_idle st

let update_evaluation_new_round st =
  let card_list =
    (st |> player_hands |> fst |> list_of_hand)
    @ (st |> dealer_hand |> list_of_hand)
  in
  List.fold_left
    (fun acc c -> evaluation.card_count <- new_card_count c)
    () card_list;
  update_evaluation_idle st

let update_evaluation_dealer st =
  let card_list = st |> dealer_hand |> list_of_hand |> List.tl in
  List.fold_left
    (fun acc c -> evaluation.card_count <- new_card_count c)
    () card_list;
  update_evaluation_idle st

let update_evaluation_split st =
  let prim, split = st |> player_hands in
  let new_cards =
    [
      prim |> list_of_hand |> List.tl |> List.hd;
      split |> list_of_hand |> List.tl |> List.hd;
    ]
  in
  List.fold_left
    (fun acc c -> evaluation.card_count <- new_card_count c)
    () new_cards;
  update_evaluation_idle st

(** [string_of_move m] is a string representation of move [m]. *)
let string_of_move m =
  match m with
  | Hit -> "Hit"
  | Stand -> "Stand"
  | Double -> "Double"
  | Split -> "Split"
  | Surrender -> "Surrender"

let string_of_evaluation () =
  "\nCURRENT EVALUATION OF GAME STATE\n" ^ "Best move: "
  ^ string_of_move evaluation.best_move
  ^ "\nRecommended Next Round Betting Factor: "
  ^ string_of_float evaluation.bet_size
  ^ "\nCurrent True Count: "
  ^ string_of_float evaluation.true_count
  ^ "\nCurrent Running Count: "
  ^ string_of_int evaluation.card_count
  ^ "\nCurrent Deck Size: "
  ^ string_of_int evaluation.deck_size
  ^ "\nCurrent Number of Decks: "
  ^ string_of_float evaluation.num_decks
