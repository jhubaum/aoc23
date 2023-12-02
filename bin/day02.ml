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

let () = 
  let games = List.map game_of_string (Aoc23.read_input "input/02.txt") in
  let possible_games = List.filter (game_is_possible part_one_bag) games in
  Printf.printf "Part 1: %d\n" (List.fold_left (fun sum game -> sum + game.id) 0 possible_games);;
