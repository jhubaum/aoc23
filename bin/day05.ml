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

let rec map_kind maps from _to value = 
  let rec find_and_apply_map it = match it with
    | [] -> raise (Failure "No map found")
    | h :: _ when h.from = from -> map_kind maps h._to _to (apply_value h value)
    | _ :: t -> find_and_apply_map t in
  if from = _to then value else find_and_apply_map maps

let rec interpret_as_range seeds = match seeds with
  | start :: length :: tl -> (List.init length ((+) start)) @ (interpret_as_range tl)
  | _ :: [] -> raise (Failure "Invalid range list")
  | [] -> [];;

let () = 
  let parsed = parse_input (Aoc23.read_input "input/05.txt") in
  let seeds = fst parsed in
  let maps = snd parsed in
  let locations1 = List.map (map_kind maps "seed" "location") seeds in
  let locations2 = List.map (map_kind maps "seed" "location") (interpret_as_range seeds) in
  printf "Part 1: %d\n" (List.fold_left min (List.hd locations1) (List.tl locations1));
  printf "Part 2: %d\n" (List.fold_left min (List.hd locations2) (List.tl locations2));;
