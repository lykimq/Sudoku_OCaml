val draw_hints : Cairo.context -> int list array array -> unit
val filter_hints : Board.t -> int list array array -> int list array array
val compute_all_hints : Board.t -> int list array array
val clear_all_hints : unit -> int list array array
