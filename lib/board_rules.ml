open Board
open Board_validation

(* Check if a row, column, or box contains all digits 1-9 without allocating
   lists *)
let is_valid_set_cells arr =
  Ui_debug.debug "Checking validity of array with length %d\n"
    (Array.length arr) ;
  flush stdout ;
  let seen = Array.make 10 false in
  let valid = ref true in
  Array.iteri
    (fun i cell ->
      Ui_debug.debug "Cell %d: %s\n" i
        (match cell with
        | Empty -> "Empty"
        | Fixed n -> Printf.sprintf "Fixed %d" n
        | Mutable n -> Printf.sprintf "Mutable %d" n) ;
      flush stdout ;
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
  Ui_debug.debug "Array validity: %b\n" !valid ;
  flush stdout ;
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
  let empty_cells = Array.exists (Array.exists (( = ) Empty)) board in
  Ui_debug.debug "Empty cells: %b\n" empty_cells ;
  flush stdout ;

  if empty_cells
  then false
  else
    (* Helper function to check if a set of cells is valid *)
    let check_set cells =
      let seen = Array.make 10 false in
      let valid = ref true in
      Array.iter
        (function
          | (Fixed n | Mutable n) when n >= 1 && n <= 9 ->
              if seen.(n) then valid := false else seen.(n) <- true
          | _ -> valid := false)
        cells ;
      !valid
    in

    (* Check all rows *)
    let rows_valid = ref true in
    for row = 0 to 8 do
      if !rows_valid
      then begin
        let valid = check_set board.(row) in
        (*Ui_debug.debug "Row %d valid: %b\n" row valid ; flush stdout ;*)
        rows_valid := valid
      end
    done ;

    (* Check all columns *)
    let cols_valid = ref true in
    for col = 0 to 8 do
      if !cols_valid
      then begin
        let column = Array.init 9 (fun row -> board.(row).(col)) in
        let valid = check_set column in
        (*Ui_debug.debug "Column %d valid: %b\n" col valid ; flush stdout ;*)
        cols_valid := valid
      end
    done ;

    (* Check all 3x3 boxes *)
    let boxes_valid = ref true in
    for box_row = 0 to 2 do
      for box_col = 0 to 2 do
        if !boxes_valid
        then begin
          let box_values =
            Array.init 9 (fun i ->
                let r = (box_row * 3) + (i / 3) in
                let c = (box_col * 3) + (i mod 3) in
                board.(r).(c))
          in
          let valid = check_set box_values in
          (*Ui_debug.debug "Box %d,%d valid: %b\n" box_row box_col valid ; flush
            stdout ;*)
          boxes_valid := valid
        end
      done
    done ;

    !rows_valid && !cols_valid && !boxes_valid
