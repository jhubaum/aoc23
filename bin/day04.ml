open Printf;;

type card = { winning: int list; actual: int list };;

let parse_card row = 
  let parse_numbers s = List.filter_map int_of_string_opt (String.split_on_char ' ' s) in
  match Str.split (Str.regexp {|\(: \)\|\( | \)|}) row with
    | _ :: winning_numbers :: actual_numbers :: [] -> { winning = (parse_numbers winning_numbers); 
                                                         actual = (parse_numbers actual_numbers) }
    | _ -> raise (Failure "Invalid Argument");;

let points_for_card acc card = 
  let number_of_winning = List.filter (fun n -> List.exists (fun w -> w = n) card.winning) card.actual in
  match List.length number_of_winning with
  | 0 -> 0
  | n -> acc n;;

let total_number_of_winning_cards cards =
  let winning_cards = List.map (points_for_card (fun n -> n)) cards in
  let rec get_n points accumulated = match points, accumulated with
  | 0, _ -> 1
  | _, [] -> 0
  | p, h::t -> h + (get_n (p-1) t) in
  let accumulated = ref [] in
  List.iter begin fun points ->
    let cur = get_n points !accumulated in
    accumulated := cur :: !accumulated;
  end (List.rev winning_cards);
  List.fold_left (+) 0 !accumulated;;

let () = 
  let cards = List.map parse_card (Aoc23.read_input "input/04.txt") in
  let pow2 n = int_of_float (2. ** (float_of_int (n-1))) in
  printf "Part 1: %d\n" (List.fold_left (+) 0 (List.map (points_for_card pow2) cards));
  printf "Part 2: %d\n" (total_number_of_winning_cards cards);;

