open Aoc2024

let puzzle_input =
  Puzzle.read_puzzle_input ()
  |> Puzzle.split_lines
  |> List.map (fun v -> String.to_seq v |> Array.of_seq)
  |> Array.of_list

let get_char_opt arr x y =
  if x >= 0 && x < Array.length arr then
    let line = arr.(x) in
    if y >= 0 && y < Array.length line then Some line.(y) else None
  else None

let get_word (x1, y1) (x2, y2) (x3, y3) (x4, y4) =
  match
    ( get_char_opt puzzle_input x1 y1,
      get_char_opt puzzle_input x2 y2,
      get_char_opt puzzle_input x3 y3,
      get_char_opt puzzle_input x4 y4 )
  with
  | Some a, Some b, Some c, Some d ->
      Some ([ a; b; c; d ] |> List.to_seq |> String.of_seq)
  | _ -> None

let get_word3 (x1, y1) (x2, y2) (x3, y3) =
  match
    ( get_char_opt puzzle_input x1 y1,
      get_char_opt puzzle_input x2 y2,
      get_char_opt puzzle_input x3 y3 )
  with
  | Some a, Some b, Some c -> Some ([ a; b; c ] |> List.to_seq |> String.of_seq)
  | _ -> None

let word_instances x y =
  let horizontal = get_word (x, y) (x, y + 1) (x, y + 2) (x, y + 3)
  and vertical = get_word (x, y) (x + 1, y) (x + 2, y) (x + 3, y)
  and diagonal_right =
    get_word (x, y) (x + 1, y + 1) (x + 2, y + 2) (x + 3, y + 3)
  and diagonal_left =
    get_word (x, y) (x - 1, y - 1) (x - 2, y - 2) (x - 3, y - 3)
  in
  List.find_all
    (fun x ->
      Option.map (fun v -> v = "XMAS" || v = "SAMX") x
      |> Option.value ~default:false)
    [ horizontal; vertical; diagonal_right; diagonal_left ]
  |> List.length

let has_cross_instance x y =
  let diagonal_a = get_word3 (x, y) (x + 1, y + 1) (x + 2, y + 2)
  and diagonal_b = get_word3 (x + 2, y) (x + 1, y + 1) (x, y + 2) in
  (diagonal_a = Some "MAS" || diagonal_a = Some "SAM")
  && (diagonal_b = Some "MAS" || diagonal_b = Some "SAM")

let part1_answer =
  puzzle_input
  |> Array.mapi (fun x arr -> Array.mapi (fun y _ -> word_instances x y) arr)
  |> Array.to_list |> Array.concat |> Array.fold_left ( + ) 0

and part2_answer =
  puzzle_input
  |> Array.mapi (fun x arr ->
         Array.mapi (fun y _ -> has_cross_instance x y) arr |> Array.to_list)
  |> Array.to_list |> List.concat |> List.filter Fun.id |> List.length

let () = Format.printf "P1: %d\nP2: %d\n" part1_answer part2_answer
