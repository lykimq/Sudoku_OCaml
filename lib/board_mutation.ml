open Board

(* Core mutation function using higher-order functions for flexible cell
   updates.

   Functional Purpose: Provides immutable board updates with compile-time safety
   guarantees and atomic operation semantics.

   Design Choice: Immutable updates over mutable in-place modification Key
   trade-offs analyzed: - Immutable (O(n²) copy): Thread safety, undo/redo
   support, referential transparency - Mutable (O(1) update): Better
   performance, less memory allocation - Decision: Chose functional purity for
   safety, debuggability, and composability

   Algorithm: Higher-order function pattern with fail-fast validation 1. Bounds
   checking - O(1) early termination 2. Cell type validation - O(1) prevent
   Fixed cell modification 3. Deep copy via Array.map Array.copy - O(n²) space
   and time 4. Function application on single cell - O(1)

   Security considerations: - Deep copy prevents race conditions in concurrent
   access - Type system prevents modification of Fixed cells (puzzle integrity)
   - Original board state preserved for rollback capability

   Alternative approaches considered: - Copy-on-write: Complex implementation,
   marginal benefit for small boards - Zipper data structure: Good for
   undo/redo, but complex API - Persistent data structures: Excellent for
   functional programming, library dependency *)
let update_cell board ~row ~col ~f =
  (* Fail fast on invalid positions *)
  if not (Board_validation.in_bounds row && Board_validation.in_bounds col)
  then None
  else
    match board.(row).(col) with
    (* Fixed cells cannot be modified - protects puzzle integrity *)
    | Fixed _ -> None
    | Empty | Mutable _ ->
        (* Immutable update: deep copy entire board *)
        let new_board = Array.map Array.copy board in
        new_board.(row).(col) <- f board.(row).(col) ;
        Some new_board

(* Clears a cell by setting it to Empty.

   Functional Purpose: Provides cell erasure functionality for user corrections.

   Design Choice: Specialized function over general-purpose update_cell call -
   Self-documenting intent (clear vs arbitrary transformation) - Type-safe -
   cannot accidentally introduce invalid values - Consistent with functional
   programming preference for named functions

   Algorithm: Delegates to update_cell with constant function - O(n²) due to
   copy

   Alternative: Could inline the logic for minor performance improvement *)
let clear_cell board ~row ~col = update_cell board ~row ~col ~f:(fun _ -> Empty)

(* Sets a cell value with two-phase validation.

   Functional Purpose: Primary user interaction function combining board
   mutation with real-time Sudoku rule validation and visual feedback.

   Design Choice: Atomic operation returning (board, validity) tuple - Single
   function call provides both state change and validation result - Prevents
   temporal coupling between update and validation - Enables immediate UI
   feedback without separate validation call

   Algorithm: Two-phase validation approach Phase 1: Structural validation
   (bounds checking, cell mutability) - Delegates to update_cell for consistency
   - Fails fast on Fixed cells or invalid coordinates

   Phase 2: Game logic validation (Sudoku constraint checking) - Uses original
   board state for validation (pre-move state) - Checks against all three
   constraint types: row, column, box - Returns new board regardless of validity
   for user feedback

   Side Effects (Pragmatic UI coupling): - Invalid_cells module updates for
   visual error highlighting - Trade-off: Pure functional vs practical user
   experience - Alternative: Could return additional data for UI updates

   Security considerations: - Input validation prevents invalid game states -
   Fixed cell protection maintains puzzle integrity - Invalid move tracking
   prevents silent failures

   Alternative approaches rejected: - Pure functional: Return Result type with
   validation info Pros: No side effects, composable Cons: Breaks atomicity,
   requires caller to handle UI updates - Exception-based: Throw on invalid
   moves Pros: Fails fast, simple success path Cons: Not functional style,
   expensive control flow - Separate validation: Validate first, then update
   Pros: Clear separation of concerns Cons: Race condition potential, duplicate
   constraint checking *)
let set_cell board ~row ~col ~value =
  match update_cell board ~row ~col ~f:(fun _ -> Mutable value) with
  | None -> None
  | Some new_board ->
      (* Validate against original board state to check if move would be
         legal *)
      if Board_validation.is_valid_move board ~row ~col ~value
      then (
        (* Side effect: Clear invalid marking (impure but pragmatic for UI
           feedback) *)
        Invalid_cells.clear_invalid ~row ~col ;
        Some (new_board, true))
      else (
        (* Side effect: Mark invalid for user feedback - prevents silent
           failures *)
        Invalid_cells.mark_invalid ~row ~col ;
        Some (new_board, false))
