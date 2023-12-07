open Printf;;

type hand_kind = HighCard | OnePair | TwoPairs | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind;;

let analyse_hand use_joker hand = 
  let rec insert_card card frequencies = match frequencies with
    | [] -> [(card, 1)]
    | (c, f) :: t -> if c = card then (c, f + 1) :: t else (c, f) :: insert_card card t in
  let frequencies = List.fold_left (fun frequencies card -> insert_card card frequencies) [] hand in
  let sorted_frequecies = List.sort (fun lhs rhs -> - compare (snd lhs) (snd rhs)) frequencies in
  let without_joker = match sorted_frequecies with
  | _ :: [] -> FiveOfAKind
  | a :: _ :: [] -> if snd a = 4 then FourOfAKind else FullHouse
  | a :: _ :: _ :: [] -> if snd a = 3 then ThreeOfAKind else TwoPairs
  | _ :: _ :: _ :: _ :: [] -> OnePair
  | l -> if List.length l = 5 then HighCard else failwith "Logic error" in
  let num_jokers = Option.value (List.find_opt (fun freq -> fst freq = 'J') frequencies |> Option.map snd) ~default:0 in
  let filtered_frequencies = List.filter (fun freq -> fst freq <> 'J') sorted_frequecies in
  let with_jokers =
    let highest_amount = match filtered_frequencies with | (_, f) :: _ -> f + num_jokers | [] -> num_jokers in
    let second_highest_freq = match filtered_frequencies with | _ :: (_, f) :: _ -> f | _ -> 0 in
    match highest_amount with
    | 1 -> HighCard
    | 2 -> if second_highest_freq = 2 then TwoPairs else OnePair
    | 3 -> if second_highest_freq = 2 then FullHouse else ThreeOfAKind
    | 4 -> FourOfAKind
    | 5 -> FiveOfAKind
    | _ -> failwith "Logic error with joker" in
  if use_joker then with_jokers else without_joker;;

let score_card use_joker card = match card with
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> if use_joker then 0 else 11
  | 'T' -> 10
  | c -> int_of_char c - int_of_char '0';;

let score_kind kind = match kind with
  | HighCard -> 0
  | OnePair -> 1
  | TwoPairs -> 2
  | ThreeOfAKind -> 3
  | FullHouse -> 4
  | FourOfAKind -> 5
  | FiveOfAKind -> 6;;

let compare_hand use_joker lhs rhs =
  let kind_score hand = hand |> analyse_hand use_joker |> score_kind in
  let scores hand = kind_score hand :: (List.map (score_card use_joker) hand) in
  let rec compare_scores lhs rhs = match lhs, rhs with
    | [], [] -> 0
    | lh :: lt, rh :: rt -> if lh = rh then compare_scores lt rt else compare lh rh
    | _ -> failwith "Logic error" in
  compare_scores (scores lhs) (scores rhs);;

let parse_line line = match String.split_on_char ' ' line with
  | hand :: score :: [] -> (List.init (String.length hand) (String.get hand), int_of_string score)
  | _ -> failwith "Invalid line";;

let () = 
  let input = List.map parse_line (Aoc23.read_input "input/07.txt") in
  let ranked use_joker = List.sort (fun lhs rhs -> compare_hand use_joker (fst lhs) (fst rhs)) input in
  let total_score use_joker = List.fold_left (+) 0 (List.mapi (fun i (_, score) -> score * (i + 1)) (ranked use_joker)) in
  printf "Part 1: %d\n" (total_score false);
  printf "Part 2: %d\n" (total_score true);;
