open Board

(* A reusable empty hints array to avoid creating new ones each time *)
let empty_hints = Array.make_matrix 9 9 []

(* Creates a copy of the empty hints array *)
let create_empty_hints () = Array.map (fun row -> Array.copy row) empty_hints

(* Creates an empty hint array with no hints shown. *)
let clear_all_hints () = empty_hints
let is_empty cell = match cell with Empty -> true | _ -> false
let is_fixed cell = match cell with Fixed _ -> true | _ -> false

(* Returns a list of all valid values (1-9) that can be placed at the specified
   position according to Sudoku rules. *)
let get_valid_numbers board ~row ~col =
  if not (Board_validation.is_valid_pos row col)
  then []
  else
    match board.(row).(col) with
    | Fixed _ | Mutable _ -> []
    | Empty ->
        (* Generate list of numbers 1-9 and filter out any that appear in same
           row, column, or box *)
        List.init 9 (fun i -> i + 1)
        |> List.filter (fun value ->
               (not (Board_validation.value_in_row board ~row ~value))
               && (not (Board_validation.value_in_col board ~col ~value))
               && not (Board_validation.value_in_box board ~row ~col ~value))

(* Calculates all valid moves for every empty cell on the board. *)
let compute_all_hints board =
  let hints = create_empty_hints () in
  for row = 0 to 8 do
    for col = 0 to 8 do
      (* Only compute hints for empty, non-fixed cells *)
      if is_empty board.(row).(col) && not (is_fixed board.(row).(col))
      then hints.(row).(col) <- get_valid_numbers board ~row ~col
    done
  done ;
  hints

(* Filters the hints array to only show hints for empty, non-fixed cells.
   Optimized to work in-place when possible. *)
let filter_hints original_board (hints_board : int list array array) :
    int list array array =
  Array.iteri
    (fun row row_array ->
      Array.iteri
        (fun col _value ->
          (* Clear hints for non-empty or fixed cells *)
          if
            (not (is_empty original_board.(row).(col)))
            || is_fixed original_board.(row).(col)
          then hints_board.(row).(col) <- [])
        row_array)
    hints_board ;
  hints_board

(* Calculates all valid moves for every empty cell on the board. This is useful
   for providing hints to the player. *)
let get_all_hints board =
  let hints = Array.make_matrix 9 9 [] in
  for row = 0 to 8 do
    for col = 0 to 8 do
      hints.(row).(col) <- get_valid_numbers board ~row ~col
    done
  done ;
  hints
