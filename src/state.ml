open Card
open Deck

type turn =
  | Dealer
  | Player
  | PlayerSplit

type h = Card.t list

type t = {
  deck : Deck.t;
  dealer_hand : h;
  player_hands : h * h;
  curr_turn : turn;
  balance : int;
  current_bet : int;
  firsthandl : bool;
  secondhandl : bool;
}

exception IllegalAction
exception NegativeBet
exception EmptyBalance

let empty_hand = []
let hand_hd (hand : h) : Card.t = List.hd hand

let init_state =
  {
    deck = empty;
    dealer_hand = empty_hand;
    player_hands = (empty_hand, empty_hand);
    curr_turn = Player;
    balance = 500;
    current_bet = 0;
    firsthandl = false;
    secondhandl = false;
  }

let add_deck st d = { st with deck = d |> combine st.deck }
let shuffle_deck st = { st with deck = shuffle st.deck }

let load_state =
  let () = Random.self_init () in
  let rand_int = 4 + Random.int 5 in
  let primary_deck =
    List.fold_left combine empty (Array.make rand_int standard |> Array.to_list)
  in
  primary_deck |> add_deck init_state |> shuffle_deck

let deck_size st = size st.deck

(** [draw_card st] is a pair containing the card draw from the deck in [st] and
    the resulting state. If there are no cards in the deck to be drawn, a
    52-card standard deck is added to [st] and shuffled prior to drawing a card. *)
let rec draw_card st =
  try
    let c, d' = draw st.deck in
    let st' = { st with deck = d' } in
    (c, st')
  with EmptyDeck -> standard |> add_deck st |> shuffle_deck |> draw_card

let start_round st =
  let c1, st' = draw_card st in
  let c2, st'' = draw_card st' in
  let dealer_card, new_state = draw_card st'' in
  let new_player_hand = [ c1; c2 ] in
  {
    new_state with
    dealer_hand = [ dealer_card ];
    player_hands = (new_player_hand, empty_hand);
    curr_turn = Player;
    firsthandl = false;
    secondhandl = false;
  }

let balance st = st.balance

let bet st n =
  if st.balance > 0 then
    if st.balance < n then raise IllegalAction
    else if n <= 0 then raise NegativeBet
    else
      let balance' = st.balance - n in
      { st with balance = balance'; current_bet = n }
  else raise EmptyBalance

let deposit st n = { st with balance = st.balance + n }
let current_bet st = st.current_bet
let current_turn st = st.curr_turn

let current_hand st =
  match st.curr_turn with
  | Dealer -> st.dealer_hand
  | Player -> fst st.player_hands
  | PlayerSplit -> snd st.player_hands

let player_hands st = st.player_hands
let dealer_hand st = st.dealer_hand

let hit st =
  let c, st' = draw_card st in
  match st.curr_turn with
  | Dealer -> (c, { st' with dealer_hand = st.dealer_hand @ [ c ] })
  | Player ->
      ( c,
        {
          st' with
          player_hands = (fst st'.player_hands @ [ c ], snd st.player_hands);
        } )
  | PlayerSplit ->
      ( c,
        {
          st' with
          player_hands = (fst st.player_hands, snd st.player_hands @ [ c ]);
        } )

(** [change_turn st] changes the turn (the resulting state with the new
    appropriate turn). *)
let change_turn st =
  match st.curr_turn with
  | Dealer -> { st with curr_turn = Player }
  | Player ->
      if snd st.player_hands = empty_hand then { st with curr_turn = Dealer }
      else { st with curr_turn = PlayerSplit }
  | PlayerSplit -> { st with curr_turn = Dealer }

let hand_size h = List.length h
let list_of_hand h = h
let stand st = change_turn st

let is_doubleable h st =
  hand_size h = 2
  && snd (player_hands st) = empty_hand
  && balance st - current_bet st >= 0

let is_surrenderable h st =
  hand_size h = 2 && snd (player_hands st) = empty_hand

let double st =
  if is_doubleable (fst st.player_hands) st then
    let st' = bet st (current_bet st) in
    { st' with current_bet = st'.current_bet * 2 }
  else raise IllegalAction

let is_splittable h st =
  is_doubleable h st
  &&
  match h with
  | [ c1; c2 ] -> is_rank c1 (rank c2)
  | _ -> failwith "Impossible pattern match"

let split st =
  if is_splittable (fst st.player_hands) st then
    let ogc1, ogc2 =
      ( st.player_hands |> fst |> hand_hd,
        st.player_hands |> fst |> List.tl |> hand_hd )
    in
    let c1, st' = draw_card st in
    let c2, st'' = draw_card st' in
    let new_balance = st.balance - st.current_bet in
    let new_crr_bet = st.current_bet * 2 in
    {
      st'' with
      player_hands = ([ ogc1; c1 ], [ ogc2; c2 ]);
      balance = new_balance;
      current_bet = new_crr_bet;
    }
  else raise IllegalAction

let surrender st =
  if is_surrenderable (fst st.player_hands) st then
    { st with balance = st.balance + (current_bet st / 2) }
  else raise IllegalAction

let rec hand_contains_rank r h =
  match h with
  | [] -> false
  | hd :: t -> is_rank hd r || hand_contains_rank r t

type value =
  | Blackjack
  | Value of int

(** [lst_product lstlst] is a list of Cartesian products of the lists in
    [lstlst]. The order of the elements in each Cartesian product lst is not
    necessarily the same as the order of appearance in the lists in [lstlst]. *)
let lst_product lstlst =
  let prods = ref [ [] ] in
  List.iter
    (fun lst ->
      prods :=
        List.map (fun h -> List.map (fun p -> h :: p) !prods) lst
        |> List.flatten)
    lstlst;
  !prods

let val_hand h =
  if hand_size h = 0 then Value 0
  else
    let values_list = List.map (fun c -> values c) h in
    let values_product = lst_product values_list in
    let lte, gt =
      List.map (fun prod -> List.fold_left ( + ) 0 prod) values_product
      |> List.partition (fun v -> v <= 21)
    in
    if List.length lte <> 0 then
      let max_val = List.fold_left max (List.hd lte) (List.tl lte) in
      if max_val = 21 && hand_size h = 2 then Blackjack else Value max_val
    else Value (List.fold_left min (List.hd gt) (List.tl gt))

type status =
  | DealerWin
  | SingleWin
  | MultiWin
  | Hand1L
  | Hand2L
  | BlackjackWin
  | Standoff
  | ContinueRound

(** [compare_val v1 v2] compares two values [v1] and [v2], and is positive if v1
    is of higher value than v2, negative if v1 is of lower value than v2, and
    zero if v1 is of equal value than v2. *)
let compare_val v1 v2 =
  match v1 with
  | Value v1' -> begin
      match v2 with
      | Value v2' ->
          if v1' > v2' then v1' - v2' else if v2' > v1' then v1' - v2' else 0
      | Blackjack -> -1
    end
  | Blackjack -> begin
      match v2 with
      | Value v2' -> 1
      | Blackjack -> 0
    end

(** [checkloss st] is a helper function used to check whether a hand was bust in
    split play *)
let checkloss st = st.firsthandl || st.secondhandl

let check_status st =
  let curr_turn = st.curr_turn in
  if curr_turn <> Dealer then
    match val_hand (current_hand st) with
    | Value v ->
        if v > 21 then
          if val_hand (snd st.player_hands) = Value 0 then DealerWin
          else if st.curr_turn = Player then Hand1L
          else if checkloss st then DealerWin
          else Hand2L
        else ContinueRound
    | Blackjack ->
        if
          hand_contains_rank Ace st.dealer_hand
          || hand_contains_rank (Number 10) st.dealer_hand
          || hand_contains_rank Jack st.dealer_hand
          || hand_contains_rank Queen st.dealer_hand
          || hand_contains_rank King st.dealer_hand
        then
          let c, st = draw_card st in
          let st' = { st with dealer_hand = st.dealer_hand @ [ c ] } in
          match val_hand st'.dealer_hand with
          | Value v -> if v = 21 then Standoff else BlackjackWin
          | Blackjack -> Standoff
        else BlackjackWin
  else
    let v1, v2, v =
      ( val_hand (fst st.player_hands),
        val_hand (snd st.player_hands),
        val_hand st.dealer_hand )
    in
    if v2 = Value 0 then
      if compare_val v (Value 21) > 0 && compare_val v Blackjack <> 0 then
        SingleWin
      else if compare_val v (Value 17) >= 0 then
        if compare_val v v1 >= 0 then DealerWin else SingleWin
      else ContinueRound
    else if compare_val v (Value 21) > 0 && compare_val v Blackjack <> 0 then
      if checkloss st then SingleWin else MultiWin
    else if compare_val v (Value 17) >= 0 then
      if checkloss st then
        if st.firsthandl then
          if compare_val v v2 >= 0 then DealerWin else SingleWin
        else if compare_val v v1 >= 0 then DealerWin
        else SingleWin
      else if compare_val v v1 >= 0 && compare_val v v2 >= 0 then DealerWin
      else if
        (compare_val v v1 >= 0 && compare_val v v2 < 0)
        || (compare_val v v1 < 0 && compare_val v v2 >= 0)
      then SingleWin
      else MultiWin
    else ContinueRound

let rec dealer_play st =
  let st' = snd (hit st) in
  let status = check_status st' in
  if status = ContinueRound then dealer_play st' else st'

let int_of_value v =
  match v with
  | Blackjack -> 21
  | Value n -> n

let string_of_value v =
  match v with
  | Blackjack -> "Blackjack"
  | Value n -> string_of_int n

let rec string_of_hand h =
  match h with
  | [] -> ""
  | [ c ] -> Card.string_of_card c
  | c :: t -> Card.string_of_card c ^ ", " ^ string_of_hand t

let firsthandloss st =
  let st' = { st with firsthandl = true; current_bet = st.current_bet / 2 } in
  st'

let secondhandloss st =
  let st' = { st with current_bet = st.current_bet / 2 } in
  st'
