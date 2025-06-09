(** Keyboard input handler for game controls and number entry.

    Design: Single callback handles all key types (arrows, numbers, special
    keys). State management: Updates selection, board state, and triggers
    redraws. Game flow: Integrates completion checking and new game generation.
*)
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

(** Mouse click handler for cell selection.

    Design: Converts screen coordinates to board positions. Filtering: Only
    processes left mouse button clicks. State: Updates selected cell and
    triggers redraw. *)
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
