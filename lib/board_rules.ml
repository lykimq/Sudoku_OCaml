open Board

let is_board_solved board =
  if Array.exists (Array.exists (( = ) Empty)) board
  then false
  else
    let valid_rows =
      Array.init 9 (fun row -> Board_validation.is_valid_row board row)
      |> Array.for_all (( = ) true)
    in
    let valid_cols =
      Array.init 9 (fun col -> Board_validation.is_valid_col board col)
      |> Array.for_all (( = ) true)
    in
    let valid_boxes =
      Array.init 9 (fun box_idx -> Board_validation.is_valid_box board box_idx)
      |> Array.for_all (( = ) true)
    in
    valid_rows && valid_cols && valid_boxes
