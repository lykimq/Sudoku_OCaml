(* Type for invalid cell state *)
type cell_state = | Invalid

(* Using Hashtbl to store invalid cells instead of maintaining a full board *)
let invalid_cells:  (int * int, cell_state) Hashtbl.t = Hashtbl.create 81

(* Mark a cell as invalid *)
let mark_invalid ~row ~col = Hashtbl.replace invalid_cells (row, col) Invalid

(* Clear an invalid mark *)
let clear_invalid ~row ~col = Hashtbl.remove invalid_cells (row, col)

(* Check if a cell is invalid *)
let is_invalid ~row ~col = Hashtbl.mem invalid_cells (row, col)
