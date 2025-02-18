type cell =
  | Empty
  | Fixed of int
  | Mutable of int

type t = cell array array

val total_size: int

val error_color: float * float * float

val create : unit -> t

val draw_board : Cairo.context -> t -> (int * int) option -> unit

val screen_to_board_pos : int -> int -> (int * int) option

val of_array : int array array -> t

val to_array : t -> int array array

val pp : Format.formatter -> t -> unit

val mark_invalid : row:int -> col:int -> unit

val clear_invalid : row:int -> col:int -> unit

val is_invalid : row:int -> col:int -> bool

val draw_hints :  Cairo.context -> int list array array -> unit

val filter_hints : t -> int list array array -> int list array array

val is_full : t -> bool

val is_valid : t -> bool
