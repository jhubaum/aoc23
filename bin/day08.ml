let parse_input lines = 
  let parse_direction line = Scanf.sscanf line "%s = (%s@, %s@)" (fun from l r -> from, (l, r)) in
  let explode s = List.init (String.length s) (String.get s) in
  match lines with
  | directions :: "" :: map -> explode directions, (List.map parse_direction map)
  | _ -> failwith "invalid input"

let find_next map step pos = 
  let next =  snd (List.find (fun m -> fst m = pos) map) in
  match step with | 'L' -> fst next | 'R' -> snd next | _ -> failwith "invalid step"


let steps_to_reach_zzz steps map =
  let rec next_step remaining_steps pos = if pos = "ZZZ" then 0 else match remaining_steps with
    | [] -> next_step steps pos
    | h :: t -> 1 + next_step t (find_next map h pos) in
  next_step steps "AAA"


let find_end_positions_on_the_way steps map start = 
  let start_positions = ref [start] in
  let counter = ref 0 in
  let step_repeated pos = List.exists ((=) pos) !start_positions in
  let rec next_step remaining_steps pos = match remaining_steps with
    | [] -> if step_repeated pos then [] else (start_positions := pos :: !start_positions; next_step steps pos)
    | h :: t -> let count = !counter in
    if String.ends_with ~suffix:"Z" pos then 
      (counter := 0; count :: next_step t (find_next map h pos)) else 
      (counter := 1 + count; next_step t (find_next map h pos)) in
  next_step steps start


let rec gcd a b =
  if b = 0 then a
  else gcd b (a mod b)

let lcd a b =
  if a = 0 || b = 0 then 0
  else (abs (a * b)) / (gcd a b)


let find_ghost_steps_count steps map = 
  let start_positions = List.filter_map 
    (fun m -> let m = fst m in if String.ends_with ~suffix:"A" m then Some m else None) map in
  let step_count pos = List.hd (find_end_positions_on_the_way steps map pos) in
  List.fold_left lcd 1 (List.map step_count start_positions)

let () = 
  let directions, map = parse_input @@ Aoc23.read_input "input/08.txt" in
  Printf.printf "Part 1: %d\n" (steps_to_reach_zzz directions map);
  Printf.printf "Part 2: %d\n" (find_ghost_steps_count directions map);;
