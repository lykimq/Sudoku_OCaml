type difficulty =
  | Easy
  | Medium
  | Hard

val generate_random_board : ?difficulty:difficulty -> unit -> int array array
val generate_easy_board : unit -> int array array
val generate_medium_board : unit -> int array array
val generate_hard_board : unit -> int array array
