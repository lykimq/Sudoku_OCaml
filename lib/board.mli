type cell =
  | Empty
  | Fixed of int
  | Mutable of int

type t = cell array array

val total_size: int

val error_color: float * float * float

val cell_size: float
val margin: float

val create : unit -> t

val draw_board : Cairo.context -> t -> (int * int) option -> unit

val screen_to_board_pos : int -> int -> (int * int) option

val of_array : int array array -> t

val to_array : t -> int array array

val pp : Format.formatter -> t -> unit
