open Board

(* Board validation utilities for Sudoku *)

(* Basic utility functions *)

(* Extracts numeric value from cell, handling all cell types safely.

   Functional Purpose: Safe conversion from sum type to option type for uniform
   processing of different cell states.

   Design Choice: Option type over exceptions - Enables functional composition
   with Option.map, List.filter_map - Prevents runtime errors from pattern match
   failures - Makes empty cells explicit in the type system

   Algorithm: Simple pattern matching - O(1) time Alternative: Could return
   Result type with error context *)
let cell_value = function Empty -> None | Fixed n | Mutable n -> Some n

(* Validates board coordinate bounds with early termination.

   Functional Purpose: Input validation for all coordinate-based operations.

   Design Choice: Simple boolean check over exceptions - Fast rejection of
   invalid inputs - Composable with other validation predicates - Clear intent
   vs throwing exceptions

   Algorithm: Two integer comparisons - O(1) time Alternative: Could use custom
   coordinate type with private constructor *)
let in_bounds i = i >= 0 && i < 9

(* Generate coordinates for validation *)

(* Generates all column coordinates for a given row.

   Functional Purpose: Creates coordinate list for row-wise validation.

   Design Choice: List.init over for-loop with accumulator - Functional style,
   no mutable state - Lazy evaluation friendly - Composable with List
   higher-order functions

   Algorithm: Maps index to coordinate tuple - O(n) time, O(n) space
   Alternative: Could use Array for O(1) access if needed frequently *)
let row_coords r = List.init 9 (fun c -> (r, c))

(* Generates all row coordinates for a given column.

   Functional Purpose: Creates coordinate list for column-wise validation.

   Design Choice: Symmetric to row_coords for consistency - Same performance
   characteristics - Predictable API design

   Algorithm: Maps index to coordinate tuple - O(n) time, O(n) space *)
let col_coords c = List.init 9 (fun r -> (r, c))

(* Generates all coordinates within a 3x3 Sudoku box.

   Functional Purpose: Maps linear box index (0-8) to 9 cell coordinates.

   Design Choice: Mathematical computation over lookup table - Memory efficient
   vs 9x9 precomputed table - Single function handles all boxes uniformly -
   Self-documenting coordinate calculation

   Algorithm: 1. Box position: (box_idx / 3 * 3, box_idx mod 3 * 3) - O(1) 2.
   Cell offset: (i / 3, i mod 3) for each of 9 cells - O(1) per cell 3.
   Coordinate: box_position + cell_offset - O(1) per cell Total: O(9) = O(1) for
   fixed-size Sudoku

   Alternative: Precomputed lookup table would be O(1) access vs O(1) compute *)
let box_coords box_idx =
  let row_start = box_idx / 3 * 3 in
  let col_start = box_idx mod 3 * 3 in
  List.init 9 (fun i ->
      let row_offset = i / 3 in
      let col_offset = i mod 3 in
      (row_start + row_offset, col_start + col_offset))

(* Determines which 3x3 box contains the given coordinate.

   Functional Purpose: Inverse mapping from coordinate to box index.

   Design Choice: Direct calculation over lookup or search - O(1) vs O(n) search
   through box coordinates - Mathematical relationship makes intent clear -
   Consistent with box_coords calculation

   Algorithm: - Box row: row / 3 (integer division) - Box col: col / 3 - Linear
   index: box_row * 3 + box_col Total: O(1) time, O(1) space

   Alternative: Could cache results in array, but overhead not worth it *)
let box_index_of (row, col) = (row / 3 * 3) + (col / 3)

(* Core validation logic *)

(* Check if a list of values contains only unique, valid Sudoku numbers.

   Functional Purpose: Universal constraint checker for rows, columns, boxes.

   Design Choice: Tail-recursive accumulator pattern - Prevents stack overflow
   on large inputs (though limited to 9 here) - Single pass algorithm for
   efficiency - Early termination on invalid values - Explicitly handles None
   (empty cells) by skipping

   Algorithm: 1. Accumulate seen values in list (could use Set for large inputs)
   2. Skip None values (empty cells don't violate constraints) 3. Fail fast on
   invalid range (1-9) 4. Fail fast on duplicates using List.mem Time: O(n²)
   worst case due to List.mem, O(n) for valid inputs Space: O(n) for accumulator

   Alternative: Could use Set.t for O(n log n) time, or bool array for O(n) *)
let has_unique_values values =
  let rec check seen = function
    | [] -> true
    | None :: rest -> check seen rest (* Skip empty cells *)
    | Some n :: _ when n < 1 || n > 9 -> false (* Invalid range *)
    | Some n :: _ when List.mem n seen -> false (* Duplicate *)
    | Some n :: rest -> check (n :: seen) rest
  in
  check [] values

(* Extract cell values at given coordinates.

   Functional Purpose: Coordinate-to-value mapping with uniform handling.

   Design Choice: List.map over imperative loop - Functional composition with
   validation functions - Preserves order of coordinates - Handles bounds errors
   naturally (would throw)

   Algorithm: Maps coordinate access to cell_value extraction - O(n) time
   Alternative: Could add bounds checking here vs. trusting caller *)
let values_at_coords board coords =
  List.map (fun (r, c) -> cell_value board.(r).(c)) coords

(* Check if a group (row/column/box) contains only unique valid values.

   Functional Purpose: High-level constraint validation combining coordinate
   generation and value checking.

   Design Choice: Function composition over monolithic implementation -
   Separates coordinate logic from validation logic - Reusable components for
   different constraint types - Clear data flow: coords -> values -> validation

   Algorithm: values_at_coords ∘ has_unique_values Time: O(n) + O(n²) = O(n²)
   where n=9 Alternative: Could inline for minor performance gain vs clarity *)
let is_valid_group board coords =
  values_at_coords board coords |> has_unique_values

(* Check if a value exists at any of the given coordinates.

   Functional Purpose: Membership test for constraint checking during moves.

   Design Choice: List.exists over manual iteration - Short-circuit evaluation
   on first match - Functional style consistent with rest of module - Clear
   intent for existence checking

   Algorithm: 1. Pattern match each coordinate's cell value 2. Early return true
   on value match 3. Continue on non-match or None Time: O(n) worst case, O(1)
   best case (early termination)

   Alternative: Could convert to Set and use Set.mem for better complexity *)
let contains_value board coords value =
  List.exists
    (fun (r, c) ->
      match cell_value board.(r).(c) with
      | Some n when n = value -> true
      | _ -> false)
    coords

(* Constraint coordinate management *)

(* Coordinate collection type for organizing constraint checking.

   Functional Purpose: Groups related coordinates for efficient batch
   operations.

   Design Choice: Record over tuple for self-documenting field access - Named
   fields vs positional access improves readability - Type system prevents field
   mixups (row_coords vs col_coords) - Enables partial application and field
   updates

   Alternative: Could use variant types or separate functions *)
type coords = (int * int) list

type constraint_coords = {
  row_coords: coords;
  col_coords: coords;
  box_coords: coords;
}

(* Pre-computes all constraint coordinates for a given cell position.

   Functional Purpose: Batch coordinate generation for move validation.

   Design Choice: Eager computation over lazy evaluation - All coordinates
   typically needed together for validation - Amortizes coordinate calculation
   cost - Simple structure for caller consumption

   Algorithm: 1. Compute box index - O(1) 2. Generate three coordinate lists -
   O(1) each 3. Package in record - O(1) Total: O(1) time, O(1) space for Sudoku

   Alternative: Could lazily compute only needed constraints *)
let get_constraint_coords row col =
  let box_idx = box_index_of (row, col) in
  {
    row_coords= row_coords row;
    col_coords= col_coords col;
    box_coords= box_coords box_idx;
  }

(* Main validation functions *)

(* Check if a move violates Sudoku constraints.

   Functional Purpose: Pre-flight validation for proposed moves to prevent
   invalid game states.

   Design Choice: Early validation pattern with short-circuit evaluation -
   Bounds check first (cheapest, most likely to fail on bad input) - Constraint
   checks second (more expensive, validate game rules) - Uses original board
   state, not hypothetical post-move state

   Algorithm: 1. Bounds validation - O(1) 2. Get constraint coordinates - O(1)
   3. Check value conflicts in row/col/box - O(n) each Total: O(n) time where
   n=9

   Alternative: Could use exception-based validation or Result types *)
let is_valid_move board ~row ~col ~value =
  if not (in_bounds row && in_bounds col)
  then false
  else
    let coords = get_constraint_coords row col in
    not
      (contains_value board coords.row_coords value
      || contains_value board coords.col_coords value
      || contains_value board coords.box_coords value)

(* Check if the entire board is completely and correctly solved.

   Functional Purpose: Win condition detection for game completion.

   Design Choice: Two-phase validation (completeness then correctness) - Early
   termination if board not full (cheapest check) - Full constraint validation
   only on complete boards - Separates concerns for clarity and efficiency

   Algorithm: 1. Completeness check: Array.exists scan - O(n²) 2. Constraint
   validation: 27 group checks (9 rows + 9 cols + 9 boxes) Each group check:
   O(n²) due to has_unique_values Total: O(n²) + 27×O(n²) = O(n²) where n=9

   Alternative: Could validate during play vs checking at end, or use
   incremental validation tracking *)
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
