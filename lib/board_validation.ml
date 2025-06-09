open Board

(** Board validation utilities for Sudoku *)

(** Basic utility functions *)
let cell_value = function Empty -> None | Fixed n | Mutable n -> Some n

let in_bounds i = i >= 0 && i < 9

(** Generate coordinates for validation *)
let row_coords r = List.init 9 (fun c -> (r, c))

let col_coords c = List.init 9 (fun r -> (r, c))

let box_coords box_idx =
  let row_start = box_idx / 3 * 3 in
  let col_start = box_idx mod 3 * 3 in
  List.init 9 (fun i ->
      let row_offset = i / 3 in
      let col_offset = i mod 3 in
      (row_start + row_offset, col_start + col_offset))

let box_index_of (row, col) = (row / 3 * 3) + (col / 3)

(** Core validation logic *)

(** Check if a list of values contains only unique, valid Sudoku numbers *)
let has_unique_values values =
  let rec check seen = function
    | [] -> true
    | None :: rest -> check seen rest (* Skip empty cells *)
    | Some n :: _ when n < 1 || n > 9 -> false (* Invalid range *)
    | Some n :: _ when List.mem n seen -> false (* Duplicate *)
    | Some n :: rest -> check (n :: seen) rest
  in
  check [] values

(** Extract cell values at given coordinates *)
let values_at_coords board coords =
  List.map (fun (r, c) -> cell_value board.(r).(c)) coords

(** Check if a group (row/column/box) contains only unique valid values *)
let is_valid_group board coords =
  values_at_coords board coords |> has_unique_values

(** Check if a value exists at any of the given coordinates *)
let contains_value board coords value =
  List.exists
    (fun (r, c) ->
      match cell_value board.(r).(c) with
      | Some n when n = value -> true
      | _ -> false)
    coords

(** Constraint coordinate management *)
type coords = (int * int) list

type constraint_coords = {
  row_coords: coords;
  col_coords: coords;
  box_coords: coords;
}

let get_constraint_coords row col =
  let box_idx = box_index_of (row, col) in
  {
    row_coords= row_coords row;
    col_coords= col_coords col;
    box_coords= box_coords box_idx;
  }

(** Main validation functions *)

(** Check if a move violates Sudoku constraints *)
let is_valid_move board ~row ~col ~value =
  if not (in_bounds row && in_bounds col)
  then false
  else
    let coords = get_constraint_coords row col in
    not
      (contains_value board coords.row_coords value
      || contains_value board coords.col_coords value
      || contains_value board coords.box_coords value)

(** Check if the entire board is completely and correctly solved *)
let is_board_solved board =
  (* Check if board is full *)
  let is_full = not (Array.exists (Array.exists (( = ) Empty)) board) in
  if not is_full
  then false
  else
    (* Check all constraints *)
    let indices = List.init 9 Fun.id in
    List.for_all (fun i -> is_valid_group board (row_coords i)) indices
    && List.for_all (fun i -> is_valid_group board (col_coords i)) indices
    && List.for_all (fun i -> is_valid_group board (box_coords i)) indices
