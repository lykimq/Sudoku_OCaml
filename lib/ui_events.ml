(* Keyboard input handler for game controls and number entry.

   Functional Purpose: Provides comprehensive keyboard interface for game
   interaction including navigation, number entry, and cell manipulation.

   Design Choice: Single unified handler over multiple specialized handlers -
   Centralized keyboard logic for consistent behavior - State-based dispatch for
   different interaction modes - Comprehensive debug logging for development and
   troubleshooting - Integration with game completion flow for seamless user
   experience

   Algorithm: Event-driven state machine with pattern matching 1. Capture GTK+
   key press events with keyval extraction 2. Pattern match on current selection
   state and key type: - Arrow keys: Navigation within board boundaries - Number
   keys: Cell value entry with validation - Special keys: Cell clearing and
   other actions 3. Update appropriate state (selection, board, UI) based on
   action 4. Trigger visual feedback through redraw scheduling 5. Handle game
   completion detection and new game flow Time: O(1) per key press +
   O(board_update) for value changes

   State management integration: - Selection state: Arrow key navigation with
   boundary checking - Board state: Number entry with validation and error
   feedback - UI state: Immediate visual feedback and hint cache management -
   Game flow: Completion detection and automatic new game generation

   Error handling and user feedback: - Invalid moves: Visual error highlighting
   with red cell background - Fixed cells: Graceful rejection of modification
   attempts - Bounds checking: Navigation constrained to valid board area -
   Debug logging: Comprehensive event tracking for development

   Alternative approaches: - Separate handlers: More modular but complex
   coordination - Command pattern: Better for undo/redo but more complex - State
   machine: More formal but potentially over-engineered *)
let handle_key_press window board_ref drawing_area key_press_handler =
  window#event#connect#key_press ~callback:(fun ev ->
      let key = GdkEvent.Key.keyval ev in
      Ui_debug.debug "Key pressed: %d\n" key ;
      Ui_debug.debug "Current selection: %s\n"
        (match !Ui_state.selected with
        | Some (row, col) -> Printf.sprintf "(%d,%d)" row col
        | None -> "None") ;

      (* Process key based on current selection and key type *)
      let result =
        match !Ui_state.selected with
        | Some (row, col) when key = GdkKeysyms._Up ->
            Ui_debug.debug "Moving selection up\n" ;
            if row > 0 then Ui_state.selected := Some (row - 1, col) ;
            GtkBase.Widget.queue_draw drawing_area#as_widget ;
            true
        | Some (row, col) when key = GdkKeysyms._Down ->
            Ui_debug.debug "Moving selection down\n" ;
            if row < 8 then Ui_state.selected := Some (row + 1, col) ;
            GtkBase.Widget.queue_draw drawing_area#as_widget ;
            true
        | Some (row, col) when key = GdkKeysyms._Left ->
            Ui_debug.debug "Moving selection left\n" ;
            if col > 0 then Ui_state.selected := Some (row, col - 1) ;
            GtkBase.Widget.queue_draw drawing_area#as_widget ;
            true
        | Some (row, col) when key = GdkKeysyms._Right ->
            Ui_debug.debug "Moving selection right\n" ;
            if col < 8 then Ui_state.selected := Some (row, col + 1) ;
            GtkBase.Widget.queue_draw drawing_area#as_widget ;
            true
        | Some (row, col) -> (
            (* Handle number keys and cell clearing *)
            match key_press_handler key with
            | Some 0 ->
                Ui_debug.debug "Attempting to clear cell at (%d,%d)\n" row col ;
                (match Board_mutation.clear_cell !board_ref ~row ~col with
                | Some new_board ->
                    Ui_debug.debug "Cell cleared successfully\n" ;
                    (* Remove error highlighting since cell is now empty *)
                    Invalid_cells.clear_invalid ~row ~col ;
                    (* Update both board references so hints display
                       correctly *)
                    board_ref := new_board ;
                    Ui_state.current_board := new_board ;
                    (* Clear cached hints - they're outdated since board
                       changed *)
                    Ui_state.current_hints := Hints.make_empty_hint_board () ;
                    GtkBase.Widget.queue_draw drawing_area#as_widget
                | None -> Ui_debug.debug "Failed to clear cell\n") ;
                true
            | Some value ->
                Ui_debug.debug "Attempting to set value %d at (%d,%d)\n" value
                  row col ;
                (match Board_mutation.set_cell !board_ref ~row ~col ~value with
                | Some (new_board, is_valid) ->
                    Ui_debug.debug "Cell updated successfully\n" ;
                    (* Update both board references so hints display
                       correctly *)
                    board_ref := new_board ;
                    Ui_state.current_board := new_board ;
                    (* Reset hints cache - placing/changing a number affects
                       possible values in other cells *)
                    Ui_state.current_hints := Hints.make_empty_hint_board () ;
                    if is_valid
                    then
                      (* Value is correct - remove any error highlighting *)
                      Invalid_cells.clear_invalid ~row ~col
                    else
                      (* Value breaks Sudoku rules - highlight cell in red *)
                      Invalid_cells.mark_invalid ~row ~col ;
                    GtkBase.Widget.queue_draw drawing_area#as_widget ;
                    (* Check for game completion and handle new game
                       generation *)
                    if Game_state.check_game_completion !board_ref
                    then begin
                      Ui_state.reset_game_state () ;
                      (* Create new board and update both references *)
                      let new_board =
                        Board.of_array (Game_state.create_new_board ())
                      in
                      board_ref := new_board ;
                      Ui_state.current_board := new_board ;
                      GtkBase.Widget.queue_draw drawing_area#as_widget
                    end
                | None -> Ui_debug.debug "Failed to update cell\n") ;
                true
            | None ->
                Ui_debug.debug "No valid key handler result\n" ;
                false)
        | None ->
            Ui_debug.debug "No valid selection\n" ;
            false
      in
      flush stdout ;
      result)

(* Mouse click handler for cell selection and user interaction.

   Functional Purpose: Translates mouse clicks to board cell selection for
   intuitive point-and-click interaction with the game board.

   Design Choice: Left-click only interaction for simplicity - Single-button
   interaction reduces complexity and user confusion - Left-click is universal
   primary action across platforms - Coordinate transformation handles margin
   and scaling automatically - Option type handling for clicks outside valid
   board area

   Algorithm: Coordinate transformation with event filtering 1. Extract mouse
   coordinates from GTK+ button press event 2. Filter events to handle only left
   mouse button (button 1) 3. Transform screen coordinates to board grid
   coordinates 4. Update selection state with new position (or None for invalid)
   5. Trigger visual feedback through redraw scheduling 6. Delegate to custom
   click handler for extensibility Time: O(1) coordinate transformation and
   state update

   Coordinate system handling: - Screen coordinates: Pixel-based from top-left
   of widget - Board coordinates: Grid-based (row, col) from top-left of board -
   Margin handling: Automatic offset for board positioning - Bounds checking:
   Graceful handling of clicks outside board

   Event filtering rationale: - Button 1 (left): Primary selection action -
   Other buttons: Ignored to prevent accidental actions - Future extensibility:
   Could add right-click context menus

   Alternative approaches: - Multi-button support: Right-click for hints, middle
   for clearing - Touch support: Gesture recognition for mobile interfaces -
   Hover effects: Visual feedback before clicking *)
let handle_mouse_click drawing_area click_handler =
  drawing_area#event#connect#button_press ~callback:(fun ev ->
      let x = int_of_float (GdkEvent.Button.x ev) in
      let y = int_of_float (GdkEvent.Button.y ev) in
      Ui_debug.debug "Raw click at (x=%d, y=%d)\n" x y ;

      let button = GdkEvent.Button.button ev in
      Ui_debug.debug "Mouse button: %d\n" button ;

      (* Only handle left clicks (button 1) *)
      if button = 1
      then begin
        let pos = Ui_board.screen_to_board_pos x y in
        Ui_debug.debug "Converting to board position: %s\n"
          (match pos with
          | Some (row, col) -> Printf.sprintf "(%d,%d)" row col
          | None -> "None") ;

        Ui_state.selected := pos ;
        Ui_debug.debug "Updated selected cell to: %s\n"
          (match !Ui_state.selected with
          | Some (row, col) -> Printf.sprintf "(%d,%d)" row col
          | None -> "None") ;

        click_handler x y ;
        GtkBase.Widget.queue_draw drawing_area#as_widget
      end ;

      flush stdout ;
      true)
