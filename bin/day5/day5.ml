open Aoc2024

let puzzle_input = Puzzle.read_puzzle_input () |> Puzzle.split_lines

let parse_rule line =
  match String.split_on_char '|' line |> List.map int_of_string with
  | [ a; b ] -> (a, b)
  | _ -> failwith "invalid rule input"

let parse_page line = String.split_on_char ',' line |> List.map int_of_string

let parse_input input =
  let rec loop (rules, pages) lines =
    match lines with
    | line :: rest when String.contains line '|' ->
        loop ([ parse_rule line ] @ rules, pages) rest
    | line :: rest when String.contains line ',' ->
        loop (rules, [ parse_page line ] @ pages) rest
    | _ :: rest -> loop (rules, pages) rest
    | [] -> (rules, pages)
  in
  loop ([], []) input

let is_correct_order rules report =
  let rec loop rest =
    let rec has_contradictions current rest =
      match rest with
      | c :: rest when List.exists (fun (a, b) -> a = current && b = c) rules ->
          has_contradictions current rest
      | _ :: _ -> true
      | [] -> false
    in
    match rest with
    | c :: rest -> if has_contradictions c rest then false else loop rest
    | [] -> true
  in
  loop report

let mean_page report =
  let length = List.length report in
  List.filteri (fun i _ -> i = length / 2) report |> List.hd

let rules, reports = parse_input puzzle_input

let part1_answer =
  List.filter (is_correct_order rules) reports
  |> List.map mean_page |> List.fold_left ( + ) 0

and part2_answer =
  List.filter (fun r -> not (is_correct_order rules r)) reports
  |> List.map
       (List.sort (fun a b ->
            if a = b then 0
            else if List.exists (fun (al, bl) -> a = al && b = bl) rules then -1
            else 1))
  |> List.map mean_page |> List.fold_left ( + ) 0

let () = Format.printf "P1: %d\nP2: %d\n" part1_answer part2_answer
