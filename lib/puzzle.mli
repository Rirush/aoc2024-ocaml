val read_puzzle_input : unit -> string
(** [read_puzzle_input ()] returns the contents of a file that has been specified as the first argument to the executable.

@raise [Failure] if executable was launched with less than 1 argument. *)

val filter_empty_strings : string list -> string list
(** [filter_empty_strings list] returns [list] without empty strings. *)

val split_lines : string -> string list
(** [split_lines str] returns a list of non-empty lines in [str]. *)
