open Board

(* Checks if the given row and column position is within
    the bounds of a 9x9 Sudoku board. *)
let is_valid_pos row col = row >= 0 && row < 9 && col >= 0 && col < 9

(* Checks if placing the given value at
    the specified position is valid according to Sudoku rules. *)
let is_valid_move board ~row ~col ~value =
  value >= 1 && value <= 9
  && (not (Validation_board.value_in_row board ~row ~value))
  && (not (Validation_board.value_in_col board ~col ~value))
  && not (Validation_board.value_in_box board ~row ~col ~value)

(* Returns a list of all valid values (1-9)
    that can be placed at the specified position according to Sudoku rules. *)
let get_valid_numbers board ~row ~col =
  if not (is_valid_pos row col)
  then []
  else
    match board.(row).(col) with
    | Fixed _ | Mutable _ -> []
    | Empty ->
        (* Generate list of numbers 1-9 and filter out any that appear in same
           row, column, or box *)
        List.init 9 (fun i -> i + 1)
        |> List.filter (fun value ->
               (not (Validation_board.value_in_row board ~row ~value))
               && (not (Validation_board.value_in_col board ~col ~value))
               && not (Validation_board.value_in_box board ~row ~col ~value))

(* Calculates all valid moves for every empty cell on the board.
    This is useful for providing hints to the player. *)
let get_all_hints board =
  let hints = Array.make_matrix 9 9 [] in
  for row = 0 to 8 do
    for col = 0 to 8 do
      hints.(row).(col) <- get_valid_numbers board ~row ~col
    done
  done ;
  hints

(* Attempts to clear a cell at the specified position.
    Fixed cells cannot be cleared. *)
let clear_cell board ~row ~col =
  if not (is_valid_pos row col)
  then None
  else
    match board.(row).(col) with
    | Fixed _ -> None (* Cannot clear fixed cells *)
    | Empty | Mutable _ ->
        let new_board = Array.map Array.copy board in
        new_board.(row).(col) <- Empty ;
        Some new_board

(* Attempts to set a cell at the specified position
    to the given value. Fixed cells cannot be modified. *)
let set_cell board ~row ~col ~value =
  if not (is_valid_pos row col)
  then None
  else
    match board.(row).(col) with
    | Fixed _ -> None (* Cannot set value in fixed cells *)
    | Empty | Mutable _ ->
        (* Create a copy of the board to modify safely *)
        let new_board = Array.map Array.copy board in
        new_board.(row).(col) <- Mutable value ;
        (* Check if the move is valid *)
        if is_valid_move board ~row ~col ~value
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

(* Represents the current status of the game *)
type game_status =
  | InProgress
  | Complete of string  (* Contains the completion message *)

(* Checks if the game has been successfully completed and returns the appropriate status. *)
let get_game_status board =
  if Validation_board.is_board_solved board
  then Complete "Congratulations! You've solved the Sudoku puzzle correctly! Would you like to start a new game?"
  else InProgress
