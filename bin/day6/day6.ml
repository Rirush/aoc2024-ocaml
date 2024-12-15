open Aoc2024

type cell = Empty | Obstacle | Guard
type direction = Up | Right | Down | Left

let next_direction = function
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up

let move direction x y =
  match direction with
  | Up -> (x - 1, y)
  | Down -> (x + 1, y)
  | Left -> (x, y - 1)
  | Right -> (x, y + 1)

let puzzle_input =
  Puzzle.read_puzzle_input ()
  |> Puzzle.split_lines
  |> List.map (fun x ->
         String.to_seq x
         |> Seq.map (function
              | '.' -> Empty
              | '#' -> Obstacle
              | '^' -> Guard
              | c -> failwith ("unknown character: " ^ String.make 1 c))
         |> Array.of_seq)
  |> Array.of_list

let starting_position =
  Array.find_mapi
    (fun x row ->
      Array.find_mapi (fun y c -> if c = Guard then Some y else None) row
      |> Option.map (fun y -> (x, y)))
    puzzle_input
  |> Option.get

let try_append arr x y v = try arr.(x).(y) <- [ v ] @ arr.(x).(y) with _ -> ()

let make_visit_matrix arr (start_x, start_y) =
  let x, y = (ref start_x, ref start_y)
  and direction = ref Up
  and max_x = Array.length arr
  and max_y = Array.length arr.(0)
  and steps = ref 0
  and visited =
    Array.make_matrix (Array.length arr) (Array.length arr.(0)) []
  in
  visited.(start_x).(start_y) <-
    [ (!direction, !steps) ] @ visited.(start_x).(start_y);
  while (!x >= 0 && !x < max_x) && !y >= 0 && !y < max_y do
    incr steps;
    let new_x, new_y = move !direction !x !y in
    let next_cell =
      try arr.(new_x).(new_y) with Invalid_argument _ -> Empty
    in
    match next_cell with
    | Empty ->
        x := new_x;
        y := new_y;
        try_append visited new_x new_y (!direction, !steps)
    | Guard ->
        x := new_x;
        y := new_y;
        try_append visited new_x new_y (!direction, !steps)
    | Obstacle ->
        direction := next_direction !direction;
        try_append visited !x !y (!direction, !steps)
  done;
  visited

let count_visited arr (start_x, start_y) =
  make_visit_matrix arr (start_x, start_y)
  |> Array.to_seq
  |> Seq.flat_map (fun x -> Array.to_seq x |> Seq.filter (fun x -> x != []))
  |> Seq.length

let has_loop _board _matrix _direction _x _y _max_step =
  (* TODO: do a test run and check if we get stuck in a loop *) false

let count_loops arr (start_x, start_y) =
  let matrix = make_visit_matrix arr (start_x, start_y)
  and loop_count = ref 0 in
  for x = 0 to Array.length matrix - 1 do
    let row = matrix.(x) in
    for y = 0 to Array.length row - 1 do
      let overlaps = row.(y) in
      List.iter
        (fun (direction, steps) ->
          if has_loop arr matrix direction x y steps then incr loop_count)
        overlaps
    done
  done;
  !loop_count

let part1_answer = count_visited puzzle_input starting_position
and part2_answer = count_loops puzzle_input starting_position

let () = Format.printf "P1: %d\nP2: %d\n" part1_answer part2_answer
