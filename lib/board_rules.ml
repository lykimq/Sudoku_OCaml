open Board

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
  Ui_debug.debug "Array validity: %b\n" !valid ;
  flush stdout ;
  !valid

let is_board_solved board =
  if Array.exists (Array.exists (( = ) Empty)) board
  then false
  else
    let valid_rows = Array.for_all is_valid_set_cells board in
    let valid_cols =
      Array.init 9 (fun col -> Array.init 9 (fun row -> board.(row).(col)))
      |> Array.for_all is_valid_set_cells
    in
    let valid_boxes =
      let extract_box board row col =
        Array.init 9 (fun i ->
            let r = (row / 3 * 3) + (i / 3) in
            let c = (col / 3 * 3) + (i mod 3) in
            board.(r).(c))
      in
      Array.init 9 Fun.id
      |> Array.for_all (fun i ->
             let row = i / 3 in
             let col = i mod 3 in
             extract_box board row col |> is_valid_set_cells)
    in
    valid_rows && valid_cols && valid_boxes
