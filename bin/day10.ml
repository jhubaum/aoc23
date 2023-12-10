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

module CoordSet = Set.Make(struct
  type t = int * int
  let compare = compare
end);;

(* Idea for part 2:
  - find enclosed areas
  - make sure that enclosed areas are not connected to the outside
    - find the two points where the loop exits the enclosed area
    - walk around the loop clockwise. For each tile, check if the
      tile to the right is outside. If it is, discard the area
*)
let find_areas map width height =
  let map = CoordSet.of_list @@ List.map fst map in
  let areas = ref [] in
  let insert_pos (x, y) =
    let check_area area = CoordSet.mem (x-1, y) area || CoordSet.mem (x, y-1) area in
    let rec iter_and_insert areas = match areas with
    | [] -> [CoordSet.singleton (x, y)]
    | area :: rest -> if check_area area 
      then (CoordSet.add (x, y) area) :: (iter_and_insert rest) 
      else area :: (iter_and_insert rest) in
    areas := iter_and_insert !areas in
  let merge_areas areas =
    let intersects lhs rhs = CoordSet.inter lhs rhs <> CoordSet.empty in
    let rec step cur rest = 
      let intersecting = List.filter (intersects cur) rest in
      let remaining = List.filter (fun r -> not (intersects cur r)) rest in
      let union = List.fold_left CoordSet.union cur intersecting in
      match remaining with 
      | [] -> [union]
      | h :: t -> union :: (step h t) in
    match areas with 
    | h :: t -> step h t
    | l -> l in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      let pos = (x, y) in
      if not (CoordSet.mem pos map) then insert_pos pos
    done
  done;
  merge_areas !areas;;

let print_areas areas = 
  List.iteri (fun i area -> printf "Area %d: %s\n" i (String.concat ", " @@ List.map (fun (x, y) -> sprintf "(%d, %d)" x y) @@ CoordSet.elements area)) areas;;

let find_enclosed_areas map width height =
  print_areas @@ find_areas map width height;;


let () = 
  let input = Aoc23.read_input "input/10_example.txt" in
  let map = parse_map input in
  let width, height = String.length (List.hd input), List.length input in
  let distance length = (length + 1) / 2 in
  printf "Part 1: %d\n" (distance (loop_length map (start map)));
  find_enclosed_areas map width height;
  (*printf "Part 2: %d\n" (enclosed_by_loop map width height);;*)
