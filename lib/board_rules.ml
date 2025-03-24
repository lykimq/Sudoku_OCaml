open Board

let is_valid_set_cells arr =
  let seen = Array.make 10 false in
  let valid = ref true in
  Array.iteri
    (fun i cell ->
      Ui_debug.debug_cell i cell ;
      match cell with
      | (Fixed n | Mutable n) when n >= 1 && n <= 9 ->
          if seen.(n)
          then begin
            Ui_debug.debug "Duplicate number %d found\n" n ;
            valid := false
          end
          else seen.(n) <- true
      | _ -> begin
          Ui_debug.debug "Invalid cell value\n" ;
          valid := false
        end)
    arr ;
  !valid

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
