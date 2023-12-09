open Printf;;

let parse_line line = List.map int_of_string (String.split_on_char ' ' line);;

let rec diff_sequence list = match list with
  | a :: b :: t -> b - a :: diff_sequence (b :: t)
  | _ -> []




let get_differentials list = 
  let rec aux list = 
    let diff = diff_sequence list in
    if List.for_all ((=) 0) diff then [] else diff :: aux diff in
  list :: aux list;;

let extrapolate_next diffs = 
  let rec last list = match list with
  | [] -> 0
  | [a] -> a
  | _ :: t -> last t in
  List.fold_left (fun i l -> i + last l) 0 diffs;;

let rec extrapolate_prev diffs = match diffs with
  | [] -> 0
  | hd :: tl -> List.hd hd - extrapolate_prev tl


let () = 
  let diff_list = List.map (fun l -> get_differentials @@ parse_line l) (Aoc23.read_input "input/09.txt") in
  let sum_predicted acc = List.fold_left (+) 0 (List.map acc diff_list) in
  printf "Part 1: %d\n" (sum_predicted extrapolate_next);
  printf "Part 2: %d\n" (sum_predicted extrapolate_prev);;
