open Board

let is_empty cell = match cell with Empty -> true | _ -> false
let is_fixed cell = match cell with Fixed _ -> true | _ -> false

(* Helper function to check if a number exists in given range *)
let value_in_row board ~row ~value =
  let rec check col =
    if col >= 9
    then false
    else
      match board.(row).(col) with
      | Empty -> check (col + 1)
      | (Fixed n | Mutable n) when n = value -> true
      | _ -> check (col + 1)
  in
  check 0

let value_in_col board ~col ~value =
  let rec check row =
    if row >= 9
    then false
    else
      match board.(row).(col) with
      | Empty -> check (row + 1)
      | (Fixed n | Mutable n) when n = value -> true
      | _ -> check (row + 1)
  in
  check 0

let value_in_box board ~row ~col ~value =
  let box_row = row / 3 * 3 in
  let box_col = col / 3 * 3 in
  let rec check_box r c =
    if r >= box_row + 3
    then false
    else if c >= box_col + 3
    then check_box (r + 1) box_col
    else
      match board.(r).(c) with
      | Empty -> check_box r (c + 1)
      | (Fixed n | Mutable n) when n = value -> true
      | _ -> check_box r (c + 1)
  in
  check_box box_row box_col

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

(* Check if a number exists in a row *)
let number_in_row board row num =
  Array.exists
    (fun cell ->
      match cell with (Fixed n | Mutable n) when n = num -> true | _ -> false)
    board.(row)

(* Check if a number exists in a column *)
let number_in_col board col num =
  Array.exists
    (fun row ->
      match row.(col) with
      | (Fixed n | Mutable n) when n = num -> true
      | _ -> false)
    board

(* Check if a number exists in a 3x3 box *)
let number_in_box board row col num =
  let start_row, start_col = (row / 3 * 3, col / 3 * 3) in
  let exists = ref false in
  for r = start_row to start_row + 2 do
    for c = start_col to start_col + 2 do
      match board.(r).(c) with
      | (Fixed n | Mutable n) when n = num -> exists := true
      | _ -> ()
    done
  done ;
  !exists

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
