open Printf;;

type range = { dest: int; start: int; range: int; };;
type map = { from: string; _to: string; ranges: range list };;

let parse_input input = 
  let parse_seeds l = (List.map int_of_string) @@ List.tl @@ (String.split_on_char ' ') @@ l in
  let parse_range l = match List.map int_of_string (String.split_on_char ' ' l) with
    | dest :: start :: range :: [] -> { dest = dest; start = start; range = range }
    | _ -> raise (Failure "Invalid range") in
  let parse_map_head l = match (String.split_on_char ' ' l) |> List.hd |> (String.split_on_char '-') with
    | from :: "to" :: _to :: [] -> (from, _to)
    | _ -> raise (Failure "Invalid map title") in
  let rec parse_ranges lines ranges = match lines with
    | [] -> ranges, []
    | "" :: t -> ranges, t
    | h :: t -> parse_ranges t ((parse_range h) :: ranges) in
  let parse_map lines = match lines with
    | [] -> raise (Failure "Invalid map")
    | h :: t -> let ranges = (parse_ranges t []) in let head = parse_map_head h in
      { from = (fst head); _to = (snd head); ranges = (fst ranges) }, (snd ranges) in
  let rec parse_maps lines = match lines with
    | [] -> []
    | lines -> let res = parse_map lines in (fst res) :: (parse_maps (snd res)) in
  match input with
    | seeds :: "" :: maps -> parse_seeds seeds, parse_maps maps
    | _ -> raise (Failure "Invalid input");;

let apply_value map value = 
  let rec iter_ranges ranges = match ranges with
    | [] -> value
    | h :: t -> 
      let diff = value - h.start in
      if diff >= 0 && diff < h.range then h.dest + diff else (iter_ranges t) in
  iter_ranges map.ranges

let apply_value_list map values = List.map (apply_value map) values;;

let rec iter_ranges (map_ranges : range list) values = 
  let map_end map_elem = map_elem.start + map_elem.range - 1 in
  let apply_elem map_elem range = 
    let diff = map_elem.start - map_elem.dest in
    match range with | a, b -> a - diff, b - diff in
  let range_outdated range value = (map_end range) < fst value in
  match map_ranges,  values with
    | [], values -> values
    | _, [] -> []
    | (mh :: mt), v when range_outdated mh (List.hd v) -> iter_ranges mt v
    | ranges, (vh :: vt) -> 
      let map = List.hd ranges in
      let tail = iter_ranges ranges in
      let not_covered = (snd vh) < map.start in
      let start_intersects = (fst vh) < map.start && (snd vh) >= map.start in
      let fully_covered = (fst vh) >= map.start && (snd vh) <= (map_end map) in
      let end_intersects = (fst vh) >= map.start && (map_end map) <= (snd vh) in
      if not_covered then
        vh :: (iter_ranges ranges vt)
      else if fully_covered then
        (apply_elem map vh) :: (tail vt)
      else if start_intersects then
        ((fst vh), map.start - 1) :: (tail ((map.start, (snd vh)) :: vt))
      else if end_intersects then
        (apply_elem map ((fst vh), (map_end map))) :: (tail (((map_end map) + 1, (snd vh)) :: vt))
      else
        raise (Failure "Logic error in iter_ranges");;

let rec merge_ranges ranges = 
  let ranges = List.sort (fun lhs rhs -> (fst lhs) - (fst rhs)) ranges in
  let should_merge lhs rhs = (snd lhs) >= (fst rhs) in
  let merge lhs rhs = (fst lhs), (snd rhs) in
  match ranges with
  | [] -> []
  | hd :: [] -> [hd]
  | lhs :: rhs :: tl -> if should_merge lhs rhs then 
    merge_ranges ((merge lhs rhs) :: tl) else 
    lhs :: rhs :: (merge_ranges tl)

let print_map map = 
  let print_range range = printf "{ dest = %d; start = %d; range = %d; }; " range.dest range.start range.range in
  print_string "Map: "; List.iter print_range map.ranges; print_newline ();;

let apply_ranges (map : map) (ranges : (int * int) list) = 
  let sorted_map = List.sort (fun lhs rhs -> rhs.start - lhs.start) map.ranges in
  let sorted_ranges = List.sort (fun lhs rhs -> (fst rhs) - (fst lhs)) ranges in
  let result = merge_ranges (iter_ranges (List.rev sorted_map) (List.rev sorted_ranges)) in
  let print_ranges start ranges = print_string start; List.iter (fun p -> printf "(%d, %d); " (fst p) (snd p)) ranges; print_newline () in
  print_map map;
  print_ranges "From: " ranges;
  print_ranges "Result: " result;
  print_newline ();
  result


let rec map_kind maps from _to apply_fct value = 
  let rec find_and_apply_map it = match it with
    | [] -> raise (Failure "No map found")
    | h :: _ when h.from = from -> printf "Map from %s to %s\n" from h._to;map_kind maps h._to _to apply_fct (apply_fct h value)
    | _ :: t -> find_and_apply_map t in
  if from = _to then value else find_and_apply_map maps;;


let rec interpret_as_range seeds = match seeds with
  | start :: length :: tl -> (start, start + length - 1) :: (interpret_as_range tl)
  | _ :: [] -> raise (Failure "Invalid range list")
  | [] -> [];;

(*
let map = { from = "seed"; _to = "soil"; ranges = [
    { dest = 60; start = 56; range = 37; };
]; };;
let seeds = [(46, 56);];;
let debug map seeds = 
  map_kind [map] "seed" "soil" apply_ranges seeds;;
*)


let () = 
  let parsed = parse_input (Aoc23.read_input "input/05.txt") in
  let seeds = fst parsed in
  let maps = snd parsed in
  let locations1 = map_kind maps "seed" "location" apply_value_list seeds in
  let locations2 = map_kind maps "seed" "location" apply_ranges (interpret_as_range seeds) in
  let min_from_ranges ranges = let starts = List.map fst ranges in 
    List.fold_left min (List.hd starts) (List.tl starts) in
  printf "Part 1: %d\n" (List.fold_left min (List.hd locations1) (List.tl locations1));
  printf "Part 2: %d\n" (min_from_ranges locations2)
