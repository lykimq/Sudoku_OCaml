open Board

val set_cell :  t ->
    row:int -> col:int -> value:int -> (t * bool) option

val clear_cell : t -> row:int -> col:int -> t option

val get_valid_numbers : t -> row:int -> col:int -> int list

val get_all_hints : t -> int list array array

(* Represents the current status of the game *)
type game_status =
  | InProgress
  | Complete of string

val get_game_status : Board.t -> game_status