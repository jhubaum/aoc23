open Printf;;

type direction = North | South | East | West;;
type tile = Direction of direction * direction | Start;;

(* TODO: This looks like a good opportunity to use a map *)
let parse_map input = 
  let parse_row y row = List.flatten (List.mapi (fun x t -> match t with
    | '.' -> []
    | '|' -> [((x, y), Direction (North, South)); ((x, y), Direction (South, North))]
    | '-' -> [((x, y), Direction (East, West)); ((x, y), Direction (West, East))]
    | 'L' -> [((x, y), Direction (North, East)); ((x, y), Direction (East, North))]
    | 'J' -> [((x, y), Direction (North, West)); ((x, y), Direction (West, North))]
    | '7' -> [((x, y), Direction (South, West)); ((x, y), Direction (West, South))]
    | 'F' -> [((x, y), Direction (South, East)); ((x, y), Direction (East, South))]
    | 'S' -> [((x, y), Start)]
    | _ -> failwith "Invalid tile"
  ) (List.init (String.length row) (String.get row))) in
  List.flatten (List.mapi parse_row input);;

let start map = fst @@ List.find (fun (_, t) -> match t with
  | Start -> true
  | _ -> false
) map;;

let apply_direction dir (x, y) = match dir with
  | North -> (x, y - 1), South
  | South -> (x, y + 1), North
  | East -> (x + 1, y), West
  | West -> (x - 1, y), East;;

let rec walk map (pos, direction) = 
  let next direction = walk map (apply_direction direction pos) in
  let apply elem = match elem with
  | Some (p, Start) -> Some [p]
  | Some (p, Direction (_, t)) -> Option.map (fun t -> p :: t) (next t)
  | None -> None in
  let matches (p, tile) = p = pos && match tile with 
    | Direction (d, _) when d == direction -> true
    | Start -> true
    | _ -> false in
  apply @@ List.find_opt matches map;;


let find_loop map start = 
  let try_dir dir = walk map (apply_direction dir start) in
  match try_dir North with
  | Some l -> l
  | None -> match try_dir South with
    | Some l -> l
    | None -> match try_dir East with
      | Some l -> l
      | None -> failwith "No loop found";;

let find_enclosed_area input loop = 
  let pipe_char_for_start = 
    let first = List.hd loop in
    let last = List.nth loop (List.length loop - 2) in
    let xdiff = fst first - fst last in
    let ydiff = snd first - snd last in
    match xdiff, ydiff with
    | -1, 1 -> 'F'
    | 1, 1 -> 'L'
    | -1, -1 -> '7'
    | (x, y) -> failwith (sprintf "Unmatched start diff x=%d, y=%d" x y) in
  let sanitized_map = 
    let sanitize y x c = match c with
    | '.' -> '.'
    | 'S' -> pipe_char_for_start
    | c -> if List.exists ((=) (x, y)) loop then c else '.' in
    let sanitize_row y row = List.mapi (sanitize y) (List.init (String.length row) (String.get row)) in
    List.mapi sanitize_row input in
  let only_pipes = 
    let rec replace_char_if_exists row to_replace char = match row with
    | [] -> to_replace
    | '-' :: t -> replace_char_if_exists t (to_replace @ ['-']) char
    | c :: t when c = char -> '|' :: (List.init (List.length to_replace) (fun _ -> ' ')) @ convert_to_pipes t
    | _ -> to_replace @ convert_to_pipes row
    and convert_to_pipes row = match row with
    | [] -> []
    | 'L' :: t -> replace_char_if_exists t ['L'] '7'
    | 'F' :: t -> replace_char_if_exists t ['F'] 'J'
    | c :: t -> c :: convert_to_pipes t in
    List.map convert_to_pipes sanitized_map in
  let count_enclosed_area row = 
    let rec step row inside = match row with
    | [] -> 0
    | '|' :: t -> step t (not inside)
    | '.' :: t -> (if inside then 1 else 0) + step t inside
    | _ :: t -> step t inside in
    step row false in
  let print_row row = print_endline @@ String.of_seq @@ List.to_seq @@ row in
  List.iter print_row only_pipes;
  List.fold_left (fun acc row -> acc + count_enclosed_area row) 0 only_pipes;;

let () = 
  let input = Aoc23.read_input "input/10.txt" in
  let map = parse_map input in
  let loop = find_loop map (start map) in
  let loop_length = (List.length loop + 1) / 2 in
  printf "Part 1: %d\n" loop_length;
  printf "Part 2: %d\n" (find_enclosed_area input loop);;
