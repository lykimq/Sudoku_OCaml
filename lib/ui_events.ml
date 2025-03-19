open GdkEvent
open Solve
open Hints
open Invalid_cells

(* Key press callback - handles all keyboard input for the game *)
let handle_key_press window board_ref drawing_area key_press_handler =
  window#event#connect#key_press ~callback:(fun ev ->
      (* Get the key value from the event *)
      let key = Key.keyval ev in
      Ui_debug.debug "Key pressed: %d\n" key ;
      Ui_debug.debug "Current selection: %s\n"
        (match !Ui_state.selected with
        | Some (row, col) -> Printf.sprintf "(%d,%d)" row col
        | None -> "None") ;

      (* Process the key press based on current selection and key value *)
      let result =
        match (!Ui_state.selected, key_press_handler key) with
        (* Case 1: Clear cell (key value is 0) *)
        | Some (row, col), Some 0 ->
            Ui_debug.debug "Attempting to clear cell at (%d,%d)\n" row col ;
            (match clear_cell !board_ref ~row ~col with
            | Some new_board ->
                Ui_debug.debug "Cell cleared successfully\n" ;
                (* Clear any invalid marking from the cell *)
                clear_invalid ~row ~col ;
                (* Update the board reference with the new state *)
                board_ref := new_board ;
                (* Reset hints since the board has changed *)
                Ui_state.current_hints := clear_all_hints () ;
                (* Request a redraw of the drawing area *)
                GtkBase.Widget.queue_draw drawing_area#as_widget
            | None -> Ui_debug.debug "Failed to clear cell\n") ;
            true
        (* Case 2: Set a value (key value is 1-9) *)
        | Some (row, col), Some value ->
            Ui_debug.debug "Attempting to set value %d at (%d,%d)\n" value row col ;
            (match set_cell !board_ref ~row ~col ~value with
            | Some (new_board, is_valid) ->
                Ui_debug.debug "Cell updated successfully\n" ;
                (* Update the board reference with the new state *)
                board_ref := new_board ;
                (* Reset hints since the board has changed *)
                Ui_state.current_hints := clear_all_hints () ;
                if is_valid
                then (
                  (* If the move is valid, clear any invalid marking *)
                  clear_invalid ~row ~col)
                else
                  (* If the move is invalid, mark the cell as invalid *)
                  mark_invalid ~row ~col ;
                (* Request a redraw of the drawing area *)
                GtkBase.Widget.queue_draw drawing_area#as_widget
            | None -> Ui_debug.debug "Failed to update cell\n") ;
            true
        (* Case 3: No valid selection or key handler result *)
        | _, _ ->
            Ui_debug.debug "No valid selection or key handler result\n" ;
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


