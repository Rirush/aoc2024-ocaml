open Aoc2024

let puzzle_input = Puzzle.read_puzzle_input ()

let find_expressions str =
  let rec loop acc str =
    match str with
    | "" -> acc
    | s when String.starts_with ~prefix:"mul(" str -> (
        let index = String.index_opt s ')' in
        match
          ( index,
            index
            |> Option.map (fun index ->
                   String.sub s 4 (index - 4)
                   |> String.split_on_char ',' |> List.map int_of_string_opt)
            |> Option.value ~default:[] )
        with
        | Some index, [ Some a; Some b ] ->
            loop
              ([ a * b ] @ acc)
              (String.sub s index (String.length s - index))
        | _, _ -> loop acc (String.sub s 4 (String.length s - 4)))
    | s -> loop acc (String.sub s 1 (String.length s - 1))
  in
  loop [] str

let filter_disabled str =
  let rec loop acc str =
    match str with
    | "" -> acc
    | s when String.starts_with ~prefix:"don't()" str ->
        loop_dont acc (String.sub s 7 (String.length s - 7))
    | s ->
        loop (acc ^ String.make 1 s.[0]) (String.sub s 1 (String.length s - 1))
  and loop_dont acc str =
    match str with
    | "" -> acc
    | s when String.starts_with ~prefix:"do()" str ->
        loop acc (String.sub s 4 (String.length s - 4))
    | s -> loop_dont acc (String.sub s 1 (String.length s - 1))
  in
  loop "" str

let part1_answer = find_expressions puzzle_input |> List.fold_left ( + ) 0

let part2_answer =
  filter_disabled puzzle_input |> find_expressions |> List.fold_left ( + ) 0

let () = Format.printf "P1: %d\nP2: %d\n" part1_answer part2_answer
