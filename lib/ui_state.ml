(* Global UI state management for the Sudoku game.

   Functional Purpose: Centralizes all mutable UI state in a single module for
   predictable state management and easy debugging.

   Design Choice: Global mutable state over functional state threading -
   Simplifies UI event handling without complex state passing - Single source of
   truth for all UI-related state - Easy integration with imperative GTK+ event
   system - Immediate state updates for responsive user interface

   Trade-offs: Functional purity vs UI pragmatism - Global state: Simple API,
   immediate updates, GTK+ compatibility - Functional state: Better testability,
   no side effects, more complex - Decision: Pragmatic approach for UI layer
   while keeping game logic pure

   Alternative: Could use functional reactive programming, state machines, or
   message-passing architecture *)

(* Current board state reference for game logic and display.

   Functional Purpose: Maintains authoritative game state for all UI operations
   and rendering decisions.

   Design Choice: Reference to board over direct board storage - Enables atomic
   board updates from external sources - Consistent with functional board
   operations (immutable updates) - Single point of truth for current game state

   Alternative: Could use event system or state synchronization *)
let current_board = ref (Board.create ())

(* Current drawing area widget reference for rendering operations.

   Functional Purpose: Provides access to GTK+ drawing widget for triggering
   redraws and handling rendering lifecycle.

   Design Choice: Option type for safe widget access - Handles initialization
   order (widget created after state module) - Prevents crashes from premature
   rendering calls - Clear indication when UI is not ready - Type-safe widget
   access throughout application

   Alternative: Could use lazy initialization or widget registry *)
let current_drawing_area : GMisc.drawing_area option ref = ref None

(* Currently selected cell coordinates for user interaction feedback.

   Functional Purpose: Tracks user focus for keyboard navigation and visual
   highlighting of active cell.

   Design Choice: Option type for optional selection state - None indicates no
   cell selected (initial state) - Some (row, col) indicates active cell for
   input - Type-safe coordinate handling prevents invalid selections - Clear
   state representation for UI logic

   Alternative: Could use special "no selection" coordinates or variant type *)
let selected : (int * int) option ref = ref None

(* Toggle for showing/hiding hints on the board.

   Functional Purpose: Controls hint display mode for user assistance without
   affecting game state or validation.

   Design Choice: Simple boolean flag over complex hint state - Binary on/off
   matches user mental model - Easy toggle implementation for menu/keyboard
   shortcuts - No performance impact when disabled - Clear separation between
   hints and game logic

   Alternative: Could use hint levels or per-cell hint control *)
let show_hints : bool ref = ref false

(* Cached hints for all empty cells to avoid recomputation.

   Functional Purpose: Performance optimization for hint display by caching
   expensive hint calculations until board state changes.

   Design Choice: Eager caching over lazy computation - UI responsiveness
   priority over memory usage - Batch computation more efficient than per-cell
   calculation - Cache invalidation on board changes maintains correctness -
   Array structure matches board layout for O(1) access

   Algorithm: Cache-aside pattern with invalidation 1. Compute all hints when
   first needed 2. Serve from cache for subsequent requests 3. Invalidate cache
   on any board modification Time: O(1) cache hits, O(n³) cache misses Space:
   O(n³) for full hint storage

   Alternative: Could use lazy evaluation, incremental updates, or compressed
   hint storage *)
let current_hints : int list array array ref =
  ref (Hints.make_empty_hint_board ())

(* Updates board and resets hints, triggering redraw if drawing area available.

   Functional Purpose: Atomic board update operation that maintains UI
   consistency by coordinating board state, hint cache, and display refresh.

   Design Choice: Atomic update operation over separate function calls -
   Prevents temporal coupling between board update and hint invalidation -
   Ensures UI consistency through single operation - Automatic redraw scheduling
   for immediate visual feedback - Handles widget availability gracefully

   Algorithm: Coordinated state update with conditional redraw 1. Update board
   reference atomically 2. Invalidate hint cache (empty board indicates "not
   computed") 3. Schedule redraw if widget available (non-blocking) Time: O(1)
   for state updates + O(1) for redraw scheduling

   GTK+ Integration: Uses queue_draw for efficient rendering - Batches multiple
   update requests into single redraw - Non-blocking operation prevents UI
   freezing - Integrates with GTK+ event loop for smooth updates

   Alternative: Could use observer pattern, event system, or manual coordination
   of updates *)
let update_board new_board =
  current_board := new_board ;
  current_hints := Hints.make_empty_hint_board () ;
  match !current_drawing_area with
  | Some drawing_area ->
      (* Schedule a redraw instead of drawing immediately - more efficient and
         prevents screen flickering from multiple rapid updates *)
      GtkBase.Widget.queue_draw drawing_area#as_widget
  | None -> ()

(* Triggers redraw of current drawing area if available.

   Functional Purpose: Provides manual refresh capability for UI updates that
   don't involve board state changes (selection, hints toggle).

   Design Choice: Conditional refresh over forced refresh - Graceful handling of
   uninitialized UI state - No-op when widget not available prevents crashes -
   Queue-based refresh for GTK+ integration - Lightweight operation suitable for
   frequent calls

   Algorithm: Conditional widget refresh 1. Check widget availability 2.
   Schedule redraw if available 3. No-op if widget not ready Time: O(1) check +
   O(1) queue operation

   Alternative: Could throw exceptions on unavailable widget or use callback
   registration system *)
let refresh_display () =
  match !current_drawing_area with
  | Some area ->
      (* Queue the refresh instead of immediate draw - GTK batches multiple
         requests together for smoother performance *)
      GtkBase.Widget.queue_draw area#as_widget
  | None -> ()

(* Resource cleanup function to prevent memory leaks.

   Functional Purpose: Explicit resource management for long-running application
   lifecycle and memory efficiency.

   Design Choice: Manual cleanup over automatic garbage collection - Immediate
   hint cache cleanup vs waiting for GC - Explicit memory management for
   predictable performance - Minor GC trigger for prompt cleanup of small
   objects - Preserves essential state (drawing area) for continued operation

   Algorithm: Selective state cleanup with GC hint 1. Reset hint cache to empty
   state 2. Trigger minor garbage collection for prompt cleanup 3. Preserve
   widget references and essential state Time: O(1) for state reset + O(GC) for
   cleanup

   Alternative: Could rely on automatic GC or use weak references *)
let cleanup_resources () =
  (* Don't clear current_drawing_area as it's needed for the game to function *)
  current_hints := Hints.make_empty_hint_board () ;
  Gc.minor () (* Trigger minor garbage collection *)

(* Resets all game state to initial values for new game.

   Functional Purpose: Comprehensive state reset for clean game restart without
   application restart.

   Design Choice: Complete state reset over partial cleanup - Ensures clean
   slate for new game - Prevents state leakage between games - Combines resource
   cleanup with state initialization - Debug logging for development and
   troubleshooting

   Algorithm: Coordinated state reset 1. Perform resource cleanup 2. Reset
   selection state to none 3. Disable hints display 4. Log reset completion for
   debugging Time: O(1) for all state resets + O(GC) for cleanup

   Alternative: Could preserve user preferences (hints) or use state machine for
   game lifecycle management *)
let reset_game_state () =
  cleanup_resources () ;
  selected := None ;
  show_hints := false ;
  Ui_debug.debug "Game state reset with resource cleanup"
