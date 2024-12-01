module IntMap = Map.Make (Int)

let puzzle_input = Aoc2024.Puzzle.read_puzzle_input ()

(* Parsing helper: turn a list of two values into a tuple of two values *)
let pair_of_list l =
  match l with
  | [ a; b ] -> (a, b)
  | _ -> failwith "list must have two elements"

(* Parsing helper: filter out empty strings *)
let not_empty v = not (String.equal "" v)
let filter_empty l = List.filter not_empty l

(* Parsing helper: process a line by parsing two ints and making a tuple of them *)
let split_numbers line =
  String.split_on_char ' ' line
  |> filter_empty |> List.map int_of_string |> pair_of_list

(* Input processing: turn input into lines, then parse lines into tuples of two ints and then split it into two int lists *)
let left_ids, right_ids =
  String.split_on_char '\n' puzzle_input
  |> filter_empty |> List.map split_numbers |> List.split

(* Sort the inputs for the part 1 solution *)
let left_ids = List.sort Int.compare left_ids
and right_ids = List.sort Int.compare right_ids

(* Part 1 specific: calculate distances between numbers and sum them up *)
let part1_result =
  List.combine left_ids right_ids
  |> List.fold_left (fun acc (l, r) -> acc + abs (l - r)) 0

(* Part 2 specific: turn a list of numbers into a map with numbers as keys and occurrence count as value *)
let count_numbers l =
  let count_reducer acc v =
    let count_updater v = 1 + Option.value ~default:0 v |> Option.some in
    IntMap.update v count_updater acc
  in
  List.fold_left count_reducer IntMap.empty l

(* Part 2 specific: turn both ID lists into count maps *)
let left_count = count_numbers left_ids
and right_count = count_numbers right_ids

(* Part 2 specific: reduce a map of occurrences into a product of counts from both maps and the corresponding key *)
let part2_result =
  let map_reducer key lc acc =
    let rc = IntMap.find_opt key right_count |> Option.value ~default:0 in
    (key * lc * rc) + acc
  in
  IntMap.fold map_reducer left_count 0

let () = Format.printf "P1: %d\nP2: %d\n" part1_result part2_result
