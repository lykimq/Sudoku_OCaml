(* Global state for tracking invalid cell positions for UI feedback.

   Functional Purpose: Provides visual error indication by maintaining a
   separate boolean matrix parallel to the game board.

   Design Choice: Global mutable state over functional alternatives Key
   trade-offs: - Global state: Simple API, immediate UI updates, no threading
   concerns - Pure functional: Would require passing state through all functions
   - Decision: Pragmatic UI concerns outweigh functional purity here

   Algorithm: Simple boolean matrix with O(1) access operations Memory: 81
   booleans = ~81 bytes vs potential kilobytes for functional approach

   Alternative approaches: - Functional: Invalid cells as part of board state or
   UI state - Event system: Publish invalid cell events for UI to handle -
   Validation cache: Store validation results with timestamps *)
let invalid_cells = Array.make_matrix 9 9 false

(* Mark a cell as invalid for UI feedback.

   Functional Purpose: Signals UI to highlight cell in error state for immediate
   user feedback on invalid moves.

   Design Choice: Direct mutation over functional update - O(1) operation vs
   O(n²) for immutable update - Immediate effect for responsive UI feedback -
   Simple API requiring only coordinates

   Algorithm: Direct array assignment - O(1) time and space

   Alternative: Could batch updates or use event system for complex UIs *)
let mark_invalid ~row ~col = invalid_cells.(row).(col) <- true

(* Clear invalid marking when cell becomes valid.

   Functional Purpose: Removes error highlighting when cell is corrected or
   cleared, maintaining accurate visual feedback.

   Design Choice: Explicit clearing over automatic detection - Caller knows when
   cell becomes valid (after successful move) - Avoids redundant validation
   checking in this module - Clear separation of concerns: this module only
   tracks, doesn't validate

   Algorithm: Direct array assignment - O(1) time and space

   Alternative: Could auto-clear by re-validating, but adds complexity *)
let clear_invalid ~row ~col = invalid_cells.(row).(col) <- false

(* Check if a cell is currently marked as invalid.

   Functional Purpose: Query interface for UI rendering to determine if cell
   should be highlighted as erroneous.

   Design Choice: Simple boolean return over complex state object - Direct
   mapping to UI needs (show error highlight: yes/no) - O(1) lookup suitable for
   frequent UI queries during rendering - No additional state manipulation
   required

   Algorithm: Direct array access - O(1) time, no space overhead

   Alternative: Could return error details or validation context *)
let is_invalid ~row ~col = invalid_cells.(row).(col)
