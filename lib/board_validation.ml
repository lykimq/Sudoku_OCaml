open Board

(* Board validation utilities for Sudoku *)

(* Extracts numeric value from cell, handling all cell types safely. *)
let cell_value = function Empty -> None | Fixed n | Mutable n -> Some n

(* Validates board coordinate bounds with early termination. *)
let in_bounds i = i >= 0 && i < 9

(* Generate coordinates for validation *)

(* Generates all column coordinates for a given row. *)
let row_coords r = List.init 9 (fun c -> (r, c))

(* Generates all row coordinates for a given column. *)
let col_coords c = List.init 9 (fun r -> (r, c))

(* Generates all coordinates within a 3x3 Sudoku box. *)
let box_coords box_idx =
  let row_start = box_idx / 3 * 3 in
  let col_start = box_idx mod 3 * 3 in
  List.init 9 (fun i ->
      let row_offset = i / 3 in
      let col_offset = i mod 3 in
      (row_start + row_offset, col_start + col_offset) )

(* Determines which 3x3 box contains the given coordinate.

   Functional Purpose: Inverse mapping from coordinate to box index.

   Algorithm: - Box row: row / 3 (integer division) - Box col: col / 3 - Linear
   index: box_row * 3 + box_col

   Total: O(1) time, O(1) space

   Alternative: Could cache results in array, but overhead not worth it *)
let box_index_of (row, col) = (row / 3 * 3) + (col / 3)

(* Core validation logic *)

(* Check if a list of values contains only unique, valid Sudoku numbers.

   Alternative: Could use Set.t for O(n log n) time, or bool array for O(n) *)
let has_unique_values values =
  let rec check seen = function
    | [] ->
        true
    | None :: rest ->
        check seen rest (* Skip empty cells *)
    | Some n :: _ when n < 1 || n > 9 ->
        false (* Invalid range *)
    | Some n :: _ when List.mem n seen ->
        false (* Duplicate *)
    | Some n :: rest ->
        check (n :: seen) rest
  in
  check [] values

(* Extract cell values at given coordinates. *)
let values_at_coords board coords =
  List.map (fun (r, c) -> cell_value board.(r).(c)) coords

(* Check if a group (row/column/box) contains only unique valid values. *)
let is_valid_group board coords =
  values_at_coords board coords |> has_unique_values

(* Check if a value exists at any of the given coordinates. *)
let contains_value board coords value =
  List.exists
    (fun (r, c) ->
      match cell_value board.(r).(c) with
      | Some n when n = value ->
          true
      | _ ->
          false )
    coords

(* Constraint coordinate management *)

(* Coordinate collection type for organizing constraint checking.
   Alternative: Could use variant types or separate functions *)
type coords = (int * int) list

type constraint_coords =
  {row_coords: coords; col_coords: coords; box_coords: coords}

(* Pre-computes all constraint coordinates for a given cell position. *)
let get_constraint_coords row col =
  let box_idx = box_index_of (row, col) in
  { row_coords= row_coords row
  ; col_coords= col_coords col
  ; box_coords= box_coords box_idx }

(* Main validation functions *)

(* Check if a move violates Sudoku constraints. *)
let is_valid_move board ~row ~col ~value =
  if not (in_bounds row && in_bounds col) then false
  else
    let coords = get_constraint_coords row col in
    not
      ( contains_value board coords.row_coords value
      || contains_value board coords.col_coords value
      || contains_value board coords.box_coords value )

(* Check if the entire board is completely and correctly solved. *)
let is_board_solved board =
  (* Check if board is full *)
  let is_full = not (Array.exists (Array.exists (( = ) Empty)) board) in
  if not is_full then false
  else
    (* Check all constraints *)
    let indices = List.init 9 Fun.id in
    List.for_all (fun i -> is_valid_group board (row_coords i)) indices
    && List.for_all (fun i -> is_valid_group board (col_coords i)) indices
    && List.for_all (fun i -> is_valid_group board (box_coords i)) indices
