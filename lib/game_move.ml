(* Checks if the given row and column position is within the bounds of a 9x9
   Sudoku board. *)
let is_valid_pos row col = row >= 0 && row < 9 && col >= 0 && col < 9

(* Checks if placing the given value at the specified position is valid
   according to Sudoku rules. *)
let is_valid_move board ~row ~col ~value =
  value >= 1 && value <= 9
  && (not (Board_validation.value_in_row board ~row ~value))
  && (not (Board_validation.value_in_col board ~col ~value))
  && not (Board_validation.value_in_box board ~row ~col ~value)
(* MOVE to board_validation.ml *)
