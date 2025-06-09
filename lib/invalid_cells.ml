(** Simple boolean array for tracking invalid cells *)
let invalid_cells = Array.make_matrix 9 9 false

(** Mark a cell as invalid for UI feedback. *)
let mark_invalid ~row ~col = invalid_cells.(row).(col) <- true

(** Clear invalid marking when cell becomes valid. *)
let clear_invalid ~row ~col = invalid_cells.(row).(col) <- false

(** Check if a cell is currently marked as invalid. *)
let is_invalid ~row ~col = invalid_cells.(row).(col)
