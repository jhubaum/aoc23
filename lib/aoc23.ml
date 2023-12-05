let read_input filename : string list =
  let lines = ref [] in
  let channel = open_in filename in
  try
    while true; do
      lines := input_line channel :: !lines
    done; !lines
  with End_of_file ->
    close_in channel;
    List.rev !lines;;


let print_list elem_to_str delim start last elements = 
  match elements with
  | [] -> print_string start; print_string last
  | t :: h -> 
      print_string start;
      print_string (elem_to_str t); 
      List.iter begin fun e -> 
        print_string delim; 
        print_string (elem_to_str e);
      end h;
      print_string last;;
