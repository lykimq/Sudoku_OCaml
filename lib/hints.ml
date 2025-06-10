open Board

(* Creates empty hint storage structure matching board dimensions.

   Functional Purpose: Initializes data structure for caching valid move hints
   across the entire board.

   Design Choice: Array of arrays with empty lists as initial values - Mirrors
   board structure for O(1) coordinate access - Empty lists indicate "not
   computed yet" vs "no valid moves" - Mutable structure for caching
   (performance pragmatism)

   Algorithm: Array.make_matrix with empty list initialization - O(n²) space

   Alternative: Could use lazy evaluation or option type to distinguish
   uncomputed vs computed-empty states *)
let make_empty_hint_board () = Array.make_matrix 9 9 []

(* Returns valid values (1-9) that can be placed at position according to Sudoku
   rules.

   Functional Purpose: Calculates all legal moves for a specific cell by
   applying constraint elimination algorithm.

   Design Choice: Optimized constraint coordinate reuse - Single coordinate
   calculation for all three constraint types - Combines row, column, box
   coordinates for unified checking - Early return for non-empty cells
   (optimization)

   Algorithm: Constraint elimination using set difference 1. Generate all
   constraint coordinates (row + col + box) - O(1) 2. Create universal set
   [1..9] - O(1) 3. Filter out values already present in constraints - O(n)
   where n=27 4. Return remaining valid values Time: O(n) where n = constraint
   size (27 for Sudoku) Space: O(n) for coordinate list and result list

   Alternative approaches: - Boolean array approach: O(1) lookup per value vs
   O(n) List.mem - Set operations: More functional but O(log n) operations -
   Separate row/col/box checking: More readable but less efficient *)
let get_valid_numbers board ~row ~col =
  match board.(row).(col) with
  | Fixed _ | Mutable _ -> []
  | Empty ->
      let coords = Board_validation.get_constraint_coords row col in
      let all_coords =
        List.concat [coords.row_coords; coords.col_coords; coords.box_coords]
      in
      List.init 9 (fun i -> i + 1)
      |> List.filter (fun value ->
             not (Board_validation.contains_value board all_coords value))

(* Calculates all possible valid moves for every empty cell.

   Functional Purpose: Batch hint computation for entire board to support hint
   display mode and move suggestion features.

   Design Choice: Eager computation over lazy evaluation - UI typically needs
   hints for multiple cells simultaneously - Batch computation amortizes
   coordinate calculation overhead - Structure matches board layout for
   intuitive access

   Algorithm: Nested Array.init with conditional hint calculation 1. For each
   cell position (i,j): - If empty: calculate valid numbers using constraint
   elimination - If filled: return empty list (no hints needed) 2. Time
   complexity: O(n⁴) worst case where n=9 - n² cells × n constraint checks × n
   value tests = O(n⁴) - In practice: O(n³) due to filled cells requiring no
   computation 3. Space complexity: O(n³) for storing all hints

   Performance trade-offs: - Eager vs Lazy: Eager chosen for UI responsiveness -
   Batch vs Individual: Batch chosen for amortized efficiency - Caching vs
   Recomputation: Caching chosen for repeated access

   Alternative approaches: - Lazy sequences: Better memory efficiency, more
   complex implementation - Incremental updates: Track hint changes after each
   move - Compressed storage: Store only cells with hints vs full grid *)
let get_all_hints board =
  Array.init 9 (fun row ->
      Array.init 9 (fun col ->
          match board.(row).(col) with
          | Empty -> get_valid_numbers board ~row ~col
          | Fixed _ | Mutable _ -> []))
