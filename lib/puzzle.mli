val read_puzzle_input : unit -> string
(** [read_puzzle_input ()] returns the contents of a file that has been specified as the first argument to the executable.

@raise [Failure] if executable was launched with less than 1 argument. *)
