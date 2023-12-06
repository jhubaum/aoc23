open Printf;;

let find_solution_space record time = 
  let t = float_of_int time in
  let r = float_of_int record in
  let s = Float.sqrt ((t *. t) -. (4. *. r)) in
  let min_value = 0.5 *. (t -. s) in
  let max_value = 0.5 *. (s +. t) in
  int_of_float (Float.ceil (min_value +. 0.001)), int_of_float (Float.floor (max_value -. 0.001))

let parse_line line = 
  match String.split_on_char ':' line with
  | _ :: numbers :: [] -> List.filter_map int_of_string_opt (String.split_on_char ' ' numbers)
  | _ -> raise (Failure "Invalid input to parse_line")

let parse_line_as_one line = 
  match String.split_on_char ':' line with
  | _ :: numbers :: [] -> int_of_string (Str.global_replace (Str.regexp " ") "" numbers)
  | _ -> raise (Failure "Invalid input to parse_line")

let main time distance = 
  let number_of_solutions pair = 
    let solution_space = find_solution_space (snd pair) (fst pair) in
      (snd solution_space) - (fst solution_space) + 1 in
  let arr_solution = List.map number_of_solutions (List.combine (parse_line time) (parse_line distance)) in
  let single_solution = number_of_solutions ((parse_line_as_one time), (parse_line_as_one distance)) in
  printf "Part 1: %d\n" (List.fold_left ( * ) 1 arr_solution);
  printf "Part 2: %d\n" single_solution;;

let () = 
  match Aoc23.read_input "input/06.txt" with 
  | time :: distance :: [] -> main time distance
  | _ -> raise (Failure "Invalid input");;

