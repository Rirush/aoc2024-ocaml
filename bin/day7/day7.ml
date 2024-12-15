open Aoc2024

type equation = int * int list

let must_split_equation line =
  match String.split_on_char ':' line with
  | [ sum; rest ] -> (sum, rest)
  | _ -> failwith "Incorrect input line"

let number_has_suffix number suffix =
  let number = string_of_int number and suffix = string_of_int suffix in
  String.ends_with ~suffix number

let remove_suffix number suffix =
  if number_has_suffix number suffix then
    let number = string_of_int number and suffix = string_of_int suffix in
    String.sub number 0 (String.length number - String.length suffix)
    |> int_of_string_opt |> Option.value ~default:0
  else number

let parse_equation line : equation =
  let sum, rest = must_split_equation line in
  let sum = int_of_string sum in
  let rest =
    String.split_on_char ' ' rest
    |> Puzzle.filter_empty_strings |> List.map int_of_string |> List.rev
  in
  (sum, rest)

let puzzle_input =
  Puzzle.read_puzzle_input () |> Puzzle.split_lines |> List.map parse_equation

let check_equation ((total, measurements) : equation) =
  let rec loop res remaining =
    match remaining with
    | h :: rest ->
        if res mod h = 0 && loop (res / h) rest then true
        else loop (res - h) rest
    | [] -> res = 0
  in
  loop total measurements

let check_equation_part2 ((total, measurements) : equation) =
  let rec loop res remaining =
    match remaining with
    | h :: rest ->
        if res mod h = 0 && loop (res / h) rest then true
        else if number_has_suffix res h && loop (remove_suffix res h) rest then
          true
        else loop (res - h) rest
    | [] -> res = 0
  in
  loop total measurements

let correct_equations = List.filter check_equation puzzle_input
and correct_equations_p2 = List.filter check_equation_part2 puzzle_input

let () =
  Format.printf "P1: %d\nP2: %d\n"
    (List.fold_left (fun acc (v, _) -> acc + v) 0 correct_equations)
    (List.fold_left (fun acc (v, _) -> acc + v) 0 correct_equations_p2)
