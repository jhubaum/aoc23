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


let loop_length map start = 
  let try_dir dir = walk map (apply_direction dir start) in
  match try_dir North with
  | Some l -> List.length l
  | None -> match try_dir South with
    | Some l -> List.length l
    | None -> match try_dir East with
      | Some l -> List.length l
      | None -> failwith "No loop found";;

let replace_nth list n elem = List.mapi (fun i e -> if i = n then elem else e) list;;

type status = Outside | Loop | Inside;;
let invert_status status = match status with
  | Outside -> Inside
  | Inside -> Outside
  | Loop -> Loop;;


module CoordSet = Set.Make(struct
  type t = int * int
  let compare = compare
end);;

module StatusMap = struct 
  (*type t = (status list) * int (* width *);;*)

  let empty width height = List.init (width * height) (fun _ -> Outside), width;;

  let get (x, y) (status_map, width) = List.nth status_map (y * width + x);;

  let update (x, y) status (status_map, width) = 
    printf "Updating (%d, %d) to %s\n" x y (match status with
      | Outside -> "Outside"
      | Inside -> "Inside"
      | Loop -> "Loop");
    replace_nth status_map (y * width + x) status, width;;

  let rec calc_x (x, y) status_map invert = if x < 0 then Outside else
    let status = get (x, y) status_map in
    let inverted_status = if invert then invert_status status else status in
    match status with 
    | Loop -> calc_x (x - 1, y) status_map (not invert)
    | _ -> inverted_status;;

  let rec calc_y (x, y) status_map invert = if y < 0 then Outside else
    let status = get (x, y) status_map in
    let inverted_status = if invert then invert_status status else status in
    match status with | Loop -> calc_y (x, y-1) status_map (not invert) | _ -> inverted_status;;

  let insert (x, y) loop status_map = 
    if CoordSet.mem (x, y) loop then update (x, y) Loop status_map else
      let x_status = calc_x (x-1, y) status_map false in
      let y_status = calc_y (x, y-1) status_map false in
      if x_status = Inside && y_status = Inside 
      then update (x, y) Inside status_map
      else update (x, y) Outside status_map;;

  let create width height loop = 
    let rec create_row y x status_map = if x = width then status_map else
      let status_map = insert (x, y) loop status_map in
      create_row y (x + 1) status_map in
    let rec create_map y status_map = if y = height then status_map else
      let status_map = create_row y 0 status_map in
      create_map (y + 1) status_map in
    create_map 0 (empty width height);;

  let count status status_map = List.length @@ List.filter (fun s -> s = status) (fst status_map);;
    
end;;

let enclosed_by_loop map width height = 
  let loop = CoordSet.of_list @@ List.map fst map in
  let status_map = StatusMap.create width height loop in
  StatusMap.count Inside status_map;;



let () = 
  let input = Aoc23.read_input "input/10_example.txt" in
  let map = parse_map input in
  let width, height = String.length (List.hd input), List.length input in
  let distance length = (length + 1) / 2 in
  printf "Part 1: %d\n" (distance (loop_length map (start map)));
  printf "Part 2: %d\n" (enclosed_by_loop map width height);;
