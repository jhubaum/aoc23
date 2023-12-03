type element = Number of int | Symbol of char;;
type point = { x: int; y: int; };;


let symbols_from_input input = 
  let number_of_digit d = (Char.code d) - (Char.code '0') in
  let current_number = ref None in
  let current_symbols = ref [] in
  let finish_number y = match !current_number with
    | None -> ()
    | Some (n, x) -> 
        current_symbols := (Number n, { x = x; y = y; }) :: !current_symbols; 
        current_number := None in
  let insert_digit x d = let d = (number_of_digit d) in
    match !current_number with
    | None -> current_number := Some (d, x)
    | Some (n, x) -> current_number := Some (n * 10 + d, x) in
  let insert_symbol s x y = current_symbols := (Symbol s, { x = x; y = y;}) :: !current_symbols in
  let inspect_char y x c = match c with
    | '.' -> finish_number y;
    | '0' .. '9' as d -> insert_digit x d;
    | h -> finish_number y; insert_symbol h x y; in
  let iter_row row y = List.iteri (inspect_char y) row; finish_number y in
  List.iteri (fun y row -> iter_row (List.init (String.length row) (String.get row)) y) input;
  !current_symbols;;

(*
let print_symbols symbols = 
  List.iter begin fun symbol ->
    let element = fst symbol in
    let p = snd symbol in 
    Printf.printf "x: %d, y: %d -> " p.x p.y;
    match element with
    | Number i -> Printf.printf "%d\n" i
    | Symbol s -> Printf.printf "%c\n" s
  end symbols;;
*)

let rec number_length number = if number < 10 then 1  else 1 + (number_length (number / 10));;

let is_near len num_start point = 
    let y_diff = point.y - num_start.y in
    let x_diff = point.x - num_start.x in
    abs y_diff <= 1 && (x_diff >= -1 && x_diff <= len);;

let numbers_with_adjacent_symbol symbols = 
  let symbol_coordinates = List.map snd (List.filter 
    (fun s -> match (fst s) with | Number _ -> false | Symbol _ -> true) symbols) in
  List.filter_map (fun s -> match (fst s) with
    | Symbol _ -> None
    | Number n -> if (List.exists (is_near (number_length n) (snd s)) symbol_coordinates) then Some n else None) symbols;;


let gear_ratios symbols = 
  let gear_points = List.filter_map (fun s -> match (fst s) with 
    | Symbol '*' -> Some (snd s)
    | _ -> None) symbols in
  let numbers_near_gear gear_point = List.filter_map (fun s -> match (fst s) with
    | Number n when is_near (number_length n) (snd s) gear_point -> Some n | _ -> None) symbols in
  List.map (fun gear_point -> match numbers_near_gear gear_point with
    | a :: b :: [] -> a * b
    | _ -> 0) gear_points


let () =
  let symbols = symbols_from_input (Aoc23.read_input "input/03.txt") in
  let sum_of_numbers_with_adj_symbol = List.fold_left (+) 0 (numbers_with_adjacent_symbol symbols) in
  Printf.printf "Part 1: %d\n" sum_of_numbers_with_adj_symbol;
  Printf.printf "Part 2: %d\n" (List.fold_left (+) 0 (gear_ratios symbols))
  
