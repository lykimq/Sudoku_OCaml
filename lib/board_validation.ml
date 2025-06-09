open Board

(** Types and utility functions for board validation. *)
type coords = (int * int) list

let cell_value = function Empty -> None | Fixed n | Mutable n -> Some n
let in_bounds i = i >= 0 && i < 9
let all_coords f = List.init 9 f

(** Coordinate generation for rows, columns, and 3x3 boxes. *)

let row_coords r = all_coords (fun c -> (r, c))
let col_coords c = all_coords (fun r -> (r, c))

let box_coords box_idx =
  let row_start, col_start = (box_idx / 3 * 3, box_idx mod 3 * 3) in
  all_coords (fun i -> (row_start + (i / 3), col_start + (i mod 3)))

(** Generic validation using uniqueness checking. *)

(** Checks if values 1-9 are unique (no duplicates) using array-based lookup.
    Algorithm: O(n) single pass with early exit on duplicates. *)
let is_unique_values cells =
  let seen = Array.make 10 false in
  try
    List.iter
      (function
        | Some n when n >= 1 && n <= 9 ->
            if seen.(n) then raise Exit else seen.(n) <- true
        | _ -> raise Exit)
      cells ;
    true
  with Exit -> false

(** Extracts cell values from board at given coordinates. *)
let extract_cells board coords =
  List.map (fun (r, c) -> cell_value board.(r).(c)) coords

(** Validates that a group (row/column/box) has unique values. *)
let is_valid_group board coords = extract_cells board coords |> is_unique_values

(** Checks if a specific value exists in the given coordinates. *)
let contains_value board coords value =
  List.exists
    (fun (r, c) ->
      match cell_value board.(r).(c) with
      | Some n when n = value -> true
      | _ -> false)
    coords

(** Validation functions for Sudoku rules. *)
let is_valid_pos row col = in_bounds row && in_bounds col

let is_valid_row board row = is_valid_group board (row_coords row)
let is_valid_col board col = is_valid_group board (col_coords col)
let is_valid_box board box_idx = is_valid_group board (box_coords box_idx)
let value_in_row board ~row ~value = contains_value board (row_coords row) value
let value_in_col board ~col ~value = contains_value board (col_coords col) value

let value_in_box board ~row ~col ~value =
  contains_value board (box_coords ((row / 3 * 3) + (col / 3))) value

(** Core validation: checks if placing a value at position violates Sudoku
    rules. *)
let is_valid_move board ~row ~col ~value =
  is_valid_pos row col
  && (not (value_in_row board ~row ~value))
  && (not (value_in_col board ~col ~value))
  && not (value_in_box board ~row ~col ~value)

(** Checks if entire board is completely and correctly solved. *)
let is_board_solved board =
  let idxs = all_coords Fun.id in
  let full = not (Array.exists (Array.exists (( = ) Empty)) board) in
  full
  && List.for_all (is_valid_row board) idxs
  && List.for_all (is_valid_col board) idxs
  && List.for_all (is_valid_box board) idxs
