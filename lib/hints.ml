open Board

let make_empty_hint_board () = Array.make_matrix 9 9 []
let is_empty cell = match cell with Empty -> true | _ -> false
let is_fixed cell = match cell with Fixed _ -> true | _ -> false

(* Returns a list of all valid values (1-9) that can be placed at the specified
   position according to Sudoku rules. *)
let get_valid_numbers board ~row ~col =
  match board.(row).(col) with
  | Fixed _ | Mutable _ -> []
  | Empty ->
      let row_vals = Board_validation.row_coords row in
      let col_vals = Board_validation.col_coords col in
      let box_vals = Board_validation.box_coords ((row / 3 * 3) + (col / 3)) in
      let all_vals = List.concat [row_vals; col_vals; box_vals] in
      List.init 9 (fun i -> i + 1)
      |> List.filter (fun value ->
             not (Board_validation.contains_value board all_vals value))

(* Filters the hints array to only show hints for empty cells. *)
let filter_hints original_board (hints_board : int list array array) :
    int list array array =
  Array.mapi
    (fun row row_vals ->
      Array.mapi
        (fun col hint ->
          if is_empty original_board.(row).(col) then hint else [])
        row_vals)
    hints_board

(* Calculates all valid moves for every empty cell on the board. This is useful
   for providing hints to the player. *)
let get_all_hints board =
  Array.init 9 (fun row ->
      Array.init 9 (fun col ->
          if is_empty board.(row).(col)
          then get_valid_numbers board ~row ~col
          else []))
