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

val mark_invalid : row:int -> col:int -> unit

val clear_invalid : row:int -> col:int -> unit

val is_invalid : row:int -> col:int -> bool

val is_empty : cell -> bool

val is_fixed : cell -> bool

val is_full : t -> bool

val is_valid : t -> bool

val is_valid_number : t -> int -> int -> int -> bool

val is_board_solved : t -> bool

(* Validation functions *)
val value_in_row : t -> row:int -> value:int -> bool
val value_in_col : t -> col:int -> value:int -> bool
val value_in_box : t -> row:int -> col:int -> value:int -> bool
