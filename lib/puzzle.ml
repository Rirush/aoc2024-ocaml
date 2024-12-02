let get_first_argument () =
  if Array.length Sys.argv < 2 then
    failwith "first argument must be a file path"
  else Sys.argv.(1)

let read_puzzle_input () =
  let file_path = get_first_argument () in
  In_channel.with_open_text file_path In_channel.input_all

let not_empty_string v = not (String.equal "" v)
let filter_empty_strings l = List.filter not_empty_string l
let split_lines str = String.split_on_char '\n' str |> filter_empty_strings
