open Board
open Validation_board

(* Check if a row, column, or box contains all digits 1-9 without allocating
   lists *)
let is_valid_set_cells arr =
  let seen = Array.make 10 false in
  let valid = ref true in
  Array.iter
    (function
      | (Fixed n | Mutable n) when n >= 1 && n <= 9 ->
          if seen.(n) then valid := false else seen.(n) <- true
      | _ -> valid := false)
    arr ;
  !valid

(* Check if a number is valid in a cell *)
let is_valid_number board row col num =
  num >= 1 && num <= 9
  && (not (number_in_row board row num))
  && (not (number_in_col board col num))
  && not (number_in_box board row col num)

(* Optimized function to check if the board is solved *)
let is_board_solved board =
  (* Quick fail: If any cell is empty, board is not solved *)
  if Array.exists (Array.exists (( = ) Empty)) board
  then false
  else
    (* Check all rows *)
    let rec check_rows row =
      row > 8 || (is_valid_set_cells board.(row) && check_rows (row + 1))
    in
    check_rows 0
    &&
    (* Check all columns *)
    let rec check_cols col =
      col > 8
      || is_valid_set_cells (Array.init 9 (fun row -> board.(row).(col)))
         && check_cols (col + 1)
    in
    check_cols 0
    &&
    (* Check all 3x3 boxes *)
    let rec check_boxes box_row box_col =
      if box_row > 2
      then true
      else if box_col > 2
      then check_boxes (box_row + 1) 0
      else
        let box_values =
          Array.init 9 (fun i ->
              let r = (box_row * 3) + (i / 3) in
              let c = (box_col * 3) + (i mod 3) in
              board.(r).(c))
        in
        is_valid_set_cells box_values && check_boxes box_row (box_col + 1)
    in
    check_boxes 0 0
