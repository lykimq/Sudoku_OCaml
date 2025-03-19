open Board

(* Attempts to clear a cell at the specified position. Fixed cells cannot be
   cleared. *)
let clear_cell board ~row ~col =
  if not (Game_move.is_valid_pos row col)
  then None
  else
    match board.(row).(col) with
    | Fixed _ -> None (* Cannot clear fixed cells *)
    | Empty | Mutable _ ->
        let new_board = Array.map Array.copy board in
        new_board.(row).(col) <- Empty ;
        Some new_board

(* Attempts to set a cell at the specified position to the given value. Fixed
   cells cannot be modified. *)
let set_cell board ~row ~col ~value =
  if not (Game_move.is_valid_pos row col)
  then None
  else
    match board.(row).(col) with
    | Fixed _ -> None (* Cannot set value in fixed cells *)
    | Empty | Mutable _ ->
        (* Create a copy of the board to modify safely *)
        let new_board = Array.map Array.copy board in
        new_board.(row).(col) <- Mutable value ;
        (* Check if the move is valid *)
        if Game_move.is_valid_move board ~row ~col ~value
        then (
          (* Clear the invalid mark if the move is valid *)
          Invalid_cells.clear_invalid ~row ~col ;
          (* Return the new board and true if the move is valid *)
          Some (new_board, true))
        else (
          (* Mark the cell as invalid if the move is not valid *)
          Invalid_cells.mark_invalid ~row ~col ;
          (* Return the new board and false if the move is not valid *)
          Some (new_board, false))
