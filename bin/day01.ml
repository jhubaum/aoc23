

let char_to_digit c = (Char.code c) - (Char.code '0');;

let extract_digits s =
  let is_digit c = match c with '0' .. '9' -> true | _ -> false in
  let digits = List.filter is_digit (List.init (String.length s) (String.get s)) in
  List.map char_to_digit digits;;

let extract_digits_with_words s =
  let rec extract chars = match chars with
    | [] -> []
    | '0' .. '9' as h :: t -> (char_to_digit h) :: extract t
    | 'o' :: ('n' :: 'e' :: _ as t)  -> 1 :: extract t
    | 't' :: ('w' :: 'o' :: _ as t) -> 2 :: extract t
    | 't' :: ('h' :: 'r' :: 'e' :: 'e' :: _ as t) -> 3 :: extract t
    | 'f' :: ('o' :: 'u' :: 'r' :: _ as t) -> 4 :: extract t
    | 'f' :: ('i' :: 'v' :: 'e' :: _ as t) -> 5 :: extract t
    | 's' :: ('i' :: 'x' :: _ as t) -> 6 :: extract t
    | 's' :: ('e' :: 'v' :: 'e' :: 'n' :: _ as t) -> 7 :: extract t
    | 'e' :: ('i' :: 'g' :: 'h' :: 't' :: _ as t) -> 8 :: extract t
    | 'n' :: ('i' :: 'n' :: 'e' :: _ as t) -> 9 :: extract t
    | _ :: t -> extract t in
  extract (List.init (String.length s) (String.get s));;

let first l = 
  match l with
  | e :: _ -> e
  | [] -> raise (Failure "Called first on empty list");;

let rec last l = 
  match l with
  | [h] -> h
  | _ :: t -> last t
  | [] -> raise (Failure "Called last on empty list");;

let calibration_value_sum extractor puzzle_input = 
  let calc_value digits = (first digits) * 10 + (last digits) in
  let calibration_value s = calc_value @@ extractor @@ s in
  List.fold_left (+) 0 (List.map calibration_value puzzle_input);;

let () = 
  print_string "Part 1: ";
  (Aoc23.read_input "input/01.txt") |> (calibration_value_sum extract_digits) |> print_int |> print_newline;

  print_string "Part 2: ";
  (Aoc23.read_input "input/01.txt") |> (calibration_value_sum extract_digits_with_words) |> print_int |> print_newline
