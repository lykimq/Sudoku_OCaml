(* Game status tracking for completion detection.

   Functional Purpose: Models the two primary game states with associated data
   for UI flow control and user messaging.

   Design Choice: Variant type with data over boolean + separate message -
   Self-documenting state transitions - Prevents invalid state combinations
   (complete without message) - Enables pattern matching for exhaustive handling
   - Completion message coupled with state for atomicity

   Alternative: Could use record type with status and message fields *)
type game_status =
  | InProgress
  | Complete of string (* Completion message *)

(* Creates a new random board with default difficulty:Easy.

   Functional Purpose: Factory function for game initialization with graceful
   fallback for board generation failures.

   Design Choice: Fallback to empty board over exception on generation failure -
   Ensures game can always start even if generation fails - User experience
   priority over pure functional error handling - Logs warning for debugging
   while providing playable state

   Algorithm: Option handling with fallback 1. Attempt board generation with
   default Easy difficulty - O(varies) 2. On success: return generated board 3.
   On failure: log warning and return empty playable board

   Security: Empty board fallback prevents application crash but impacts UX

   Alternative approaches: - Exception-based: Throw on generation failure (less
   robust) - Result type: Return Result for caller to handle (more functional) -
   Retry logic: Attempt generation multiple times before fallback *)
let create_new_board () =
  match Board_generate.generate_safe () with
  | Some board -> board
  | None ->
      (* Fallback to a basic empty board if generation fails *)
      Printf.eprintf "Warning: Board generation failed, creating empty board\n" ;
      Array.make_matrix 9 9 0

(* Checks board completion and returns appropriate status with message.

   Functional Purpose: Evaluates win condition and provides user-friendly
   completion feedback for UI display.

   Design Choice: Combined status and message generation - Single function
   provides both game state and UI text - Consistent messaging vs scattered
   string literals - Enables easy message customization and localization

   Algorithm: Delegation to validation with status mapping 1. Board completion
   check via Board_validation.is_board_solved - O(n²) 2. Status mapping with
   hardcoded message (could be externalized) 3. Debug logging for development
   and troubleshooting

   Side Effects: Debug output for development visibility

   Alternative: Could separate completion checking from message generation *)
let get_game_status board =
  let solved = Board_validation.is_board_solved board in
  Ui_debug.debug "Solved: %b\n" solved ;
  flush stdout ;

  if solved
  then
    Complete
      "Congratulations! You've solved the Sudoku puzzle correctly! Would you \
       like to start a new game?"
  else InProgress

(* Checks for game completion and shows dialog if solved.

   Functional Purpose: Integrates game state checking with UI dialog management
   and new game flow, providing complete win condition handling.

   Design Choice: Combined completion check and UI handling - Atomic operation:
   check + UI response + return action - Exception handling for robustness in UI
   operations - Boolean return indicates whether new game was started

   Algorithm: Multi-phase game completion flow 1. Game status evaluation - O(n²)
   for board validation 2. Pattern matching on status for appropriate action 3.
   UI dialog display and user response handling 4. Return boolean indicating
   game flow continuation

   Error Handling: Try-catch wrapper for UI operation safety - UI operations can
   fail (display issues, user cancellation) - Graceful failure prevents game
   state corruption - Debug logging for troubleshooting UI issues

   Side Effects: - Debug output for development visibility - UI dialog creation
   and interaction - Potential game state reset (handled by UI component)

   Alternative approaches: - Separate functions: check completion, show dialog,
   handle response - Event-based: Publish completion event for UI to handle -
   Callback-based: Accept callback for completion handling *)
let check_game_completion board =
  Ui_debug.debug "Checking game completion\n" ;
  flush stdout ;

  try
    match get_game_status board with
    | Complete message ->
        Ui_debug.debug "Game completed: %s\n" message ;
        flush stdout ;
        Ui_game_completion.show_completion_dialog message
    | InProgress ->
        Ui_debug.debug "Game in progress\n" ;
        flush stdout ;
        false
  with e ->
    Ui_debug.debug "Error in game completion check: %s\n" (Printexc.to_string e) ;
    flush stdout ;
    false
