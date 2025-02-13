type cell =
  | Empty
  | Fixed of int
  | Mutable of int

type t = cell array array

val create : unit -> t

val draw_board : Cairo.context -> t -> (int * int) option -> unit

val screen_to_board_pos : int -> int -> (int * int) option

val of_array : int array array -> t

val set_cell :  t ->
    row:int -> col:int -> value:int -> cell array array option

val get_empty_pos : t -> (int * int) list

val is_completed : t -> bool

val get_cell : t -> row:int -> col:int -> cell

val to_array : t -> int array array

val total_size: int
val error_color: float * float * float
val fixed_color: float * float * float
val mutable_color: float * float * float
val selected_color: float * float * float

val pp : Format.formatter -> t -> unit
