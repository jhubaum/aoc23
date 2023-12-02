(* I have no idea how the map module works, so I'm using a list *)
type round = (string * int) list;;

type game = {
  id: int;
  rounds: round list;
};;

let game_of_string s = 
  let game_id s = int_of_string (Str.string_after s 5) in
  let color_and_amount s = match Str.split (Str.regexp " ") s with
    | amount :: color :: [] -> (color, (int_of_string amount))
    | _ -> raise (Failure "Invalid group") in
  let get_rounds s = List.map color_and_amount (Str.split (Str.regexp ", ?") s) in
  match Str.split (Str.regexp "[:;]") s with
  | game :: rounds -> { id = (game_id game); rounds = List.map get_rounds rounds }
  | [] -> raise (Failure "Invalid game string");;

(*
let print_game g = 
  print_string "Game ";
  print_int g.id;
  print_string ": ";
  List.iter (fun round ->
    List.iter (fun r -> print_int (snd r); print_string (fst r); print_char ',' ) round;
    print_string ";"
  ) g.rounds;
  print_endline "";;
*)

let part_one_bag = [ ("red", 12); ("green", 13); ("blue", 14) ];;

let game_is_possible bag game = 
  let check_elem elem = List.exists (fun b -> (fst b) = (fst elem) && (snd b) >= (snd elem)) bag in
  let check_round round = List.for_all check_elem round in
  List.for_all check_round game.rounds;;

let minimum_bag game =
  let rec for_elem bag color amount = match bag with
  | [] -> [(color, amount)]
  | (c, a) :: t when c = color && a < amount -> (color, amount) :: t 
  | (c, a) :: t when c = color -> (c, a) :: t
  | h :: t -> h :: (for_elem t color amount) in
  let for_round bag round = List.fold_left (fun bag r -> for_elem bag (fst r) (snd r)) bag round in
  List.fold_left (fun bag r -> for_round bag r) [] game.rounds;;

let power_of_bag bag =
  let amount_for_color color = Option.value
    (Option.map snd (List.find_opt (fun e -> (fst e) = color) bag)) 
    ~default: 0 in
  (amount_for_color "red") * (amount_for_color "blue") * (amount_for_color "green");;


let () = 
  let games = List.map game_of_string (Aoc23.read_input "input/02.txt") in
  let possible_games = List.filter (game_is_possible part_one_bag) games in
  let minimum_bags = List.map minimum_bag games in
  Printf.printf "Part 1: %d\n" (List.fold_left (fun sum game -> sum + game.id) 0 possible_games);
  Printf.printf "Part 2: %d\n" (List.fold_left (+) 0 (List.map power_of_bag minimum_bags));;
