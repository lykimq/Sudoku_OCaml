open Board

val set_cell :  t ->
    row:int -> col:int -> value:int -> (t * bool) option

val clear_cell : t -> row:int -> col:int -> t option

val get_all_hints : t -> int list array array
