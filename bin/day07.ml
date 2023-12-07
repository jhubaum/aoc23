open Printf;;

type hand_kind = HighCard | OnePair | TwoPairs | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind;;

let analyse_hand hand = 
  let rec insert_card card frequencies = match frequencies with
    | [] -> [(card, 1)]
    | (c, f) :: t -> if c = card then (c, f + 1) :: t else (c, f) :: insert_card card t in
  let frequencies = List.fold_left (fun frequencies card -> insert_card card frequencies) [] hand in
  match List.sort (fun lhs rhs -> - compare (snd lhs) (snd rhs)) frequencies with
  | _ :: [] -> FiveOfAKind
  | a :: _ :: [] -> if snd a = 4 then FourOfAKind else FullHouse
  | a :: _ :: _ :: [] -> if snd a = 3 then ThreeOfAKind else TwoPairs
  | _ :: _ :: _ :: _ :: [] -> OnePair
  | l -> if List.length l = 5 then HighCard else failwith "Logic error";;


let score_card card = match card with
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> 11
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

let compare_hand lhs rhs =
  let kind_score hand = hand |> analyse_hand |> score_kind in
  let scores hand = kind_score hand :: List.map score_card hand in
  let rec compare_scores lhs rhs = match lhs, rhs with
    | [], [] -> 0
    | lh :: lt, rh :: rt -> if lh = rh then compare_scores lt rt else compare lh rh
    | _ -> failwith "Logic error" in
  compare_scores (scores lhs) (scores rhs);;

let parse_line line = match String.split_on_char ' ' line with
  | hand :: score :: [] -> (List.init (String.length hand) (String.get hand), int_of_string score)
  | _ -> failwith "Invalid line";;

let () = 
  let input = List.map parse_line (Aoc23.read_input "input/07_example.txt") in
  let ranked = List.sort (fun lhs rhs -> compare_hand (fst lhs) (fst rhs)) input in
  let total_score = List.fold_left (+) 0 (List.mapi (fun i (_, score) -> score * (i + 1)) ranked) in
  printf "Part 1: %d\n" total_score;;
