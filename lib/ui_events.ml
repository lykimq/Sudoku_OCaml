(* Key press callback - handles all keyboard input for the game *)
let handle_key_press window board_ref drawing_area key_press_handler =
  window#event#connect#key_press ~callback:(fun ev ->
      (* Get the key value from the event *)
      let key = GdkEvent.Key.keyval ev in
      Ui_debug.debug "Key pressed: %d\n" key ;
      Ui_debug.debug "Current selection: %s\n"
        (match !Ui_state.selected with
        | Some (row, col) -> Printf.sprintf "(%d,%d)" row col
        | None -> "None") ;

      (* Process the key press based on current selection and key value *)
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
            (* Handle numeric keys and other inputs *)
            match key_press_handler key with
            | Some 0 ->
                Ui_debug.debug "Attempting to clear cell at (%d,%d)\n" row col ;
                (match Board_mutation.clear_cell !board_ref ~row ~col with
                | Some new_board ->
                    Ui_debug.debug "Cell cleared successfully\n" ;
                    (* Clear any invalid marking from the cell *)
                    Invalid_cells.clear_invalid ~row ~col ;
                    (* Update the board reference with the new state *)
                    board_ref := new_board ;
                    (* Reset hints since the board has changed *)
                    Ui_state.current_hints := Hints.make_empty_hint_board () ;
                    (* Request a redraw of the drawing area *)
                    GtkBase.Widget.queue_draw drawing_area#as_widget
                | None -> Ui_debug.debug "Failed to clear cell\n") ;
                true
            | Some value ->
                Ui_debug.debug "Attempting to set value %d at (%d,%d)\n" value
                  row col ;
                (match Board_mutation.set_cell !board_ref ~row ~col ~value with
                | Some (new_board, is_valid) ->
                    Ui_debug.debug "Cell updated successfully\n" ;
                    (* Update the board reference with the new state *)
                    board_ref := new_board ;
                    (* Reset hints since the board has changed *)
                    Ui_state.current_hints := Hints.make_empty_hint_board () ;
                    if is_valid
                    then
                      (* If the move is valid, clear any invalid marking *)
                      Invalid_cells.clear_invalid ~row ~col
                    else
                      (* If the move is invalid, mark the cell as invalid *)
                      Invalid_cells.mark_invalid ~row ~col ;
                    (* Request a redraw of the drawing area *)
                    GtkBase.Widget.queue_draw drawing_area#as_widget ;
                    (* Check for game completion after a valid move *)
                    if Game_state.check_game_completion !board_ref
                    then begin
                      (* If user wants to start a new game *)
                      Ui_state.reset_game_state () ;
                      board_ref :=
                        Board.of_array
                          (Board_generate.generate_random_board
                             ~difficulty:Board_generate.Easy ()) ;
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
      (* Ensure debug output is flushed to stdout *)
      flush stdout ;
      (* Return whether the key press was handled *)
      result)

(* Mouse click callback *)
let handle_mouse_click drawing_area click_handler =
  drawing_area#event#connect#button_press ~callback:(fun ev ->
      let x = int_of_float (GdkEvent.Button.x ev) in
      let y = int_of_float (GdkEvent.Button.y ev) in
      Ui_debug.debug "Raw click at (x=%d, y=%d)\n" x y ;

      (* Add event button check *)
      let button = GdkEvent.Button.button ev in
      Ui_debug.debug "Mouse button: %d\n" button ;

      (* Make sure we're only handling left clicks *)
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
