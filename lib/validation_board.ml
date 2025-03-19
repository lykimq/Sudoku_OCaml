open Board

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
