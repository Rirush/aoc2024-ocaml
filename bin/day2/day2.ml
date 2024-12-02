open Aoc2024

type level_order = Increasing | Decreasing

let parse_numbers line =
  String.split_on_char ' ' line
  |> Puzzle.filter_empty_strings |> List.map int_of_string

let count_if predicate list =
  let reducer acc value = if predicate value then acc + 1 else acc in
  List.fold_left reducer 0 list

let is_safe_report report =
  let rec loop = function
    | Decreasing, a :: b :: _ when a <= b || a - b > 3 -> false
    | Increasing, a :: b :: _ when a >= b || b - a > 3 -> false
    | _, [] -> true
    | order, _ :: rest -> loop (order, rest)
  in
  match report with
  | a :: b :: _ when a > b -> loop (Decreasing, report)
  | a :: b :: _ when a < b -> loop (Increasing, report)
  | _ -> false

let is_safe_report_dampened report =
  let check_without index _ =
    List.filteri (fun i _ -> i != index) report |> is_safe_report
  in
  match is_safe_report report with
  | true -> true
  | false -> List.mapi check_without report |> List.exists Fun.id

let reports =
  Aoc2024.Puzzle.read_puzzle_input ()
  |> Puzzle.split_lines |> List.map parse_numbers

let part1_answer = reports |> List.map is_safe_report |> count_if Fun.id

let part2_answer =
  reports |> List.map is_safe_report_dampened |> count_if Fun.id

let () = Printf.printf "P1: %d\nP2: %d\n" part1_answer part2_answer
