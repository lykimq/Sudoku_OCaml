open Board

let update_cell board ~row ~col ~f =
  if not (Game_move.is_valid_pos row col)
  then None
  else
    match board.(row).(col) with
    | Fixed _ -> None
    | Empty | Mutable _ ->
        let new_board = Array.map Array.copy board in
        new_board.(row).(col) <- f board.(row).(col) ;
        Some new_board

(* Attempts to clear a cell at the specified position. Fixed cells cannot be
   cleared. *)
let clear_cell board ~row ~col = update_cell board ~row ~col ~f:(fun _ -> Empty)

(* Attempts to set a cell at the specified position to the given value. Fixed
   cells cannot be modified. *)
let set_cell board ~row ~col ~value =
  match update_cell board ~row ~col ~f:(fun _ -> Mutable value) with
  | None -> None
  | Some new_board ->
      (* Check valid move of the original board *)
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
