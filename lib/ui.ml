open GMain

let check_game_completion board =
  match Solve.get_game_status board with
  | Solve.Complete message ->
      let dialog =
        GWindow.message_dialog
          ~message
          ~message_type:`INFO ~buttons:GWindow.Buttons.ok ~title:"Game Complete"
          ()
      in
      ignore (dialog#run ()) ;
      dialog#destroy ()
  | Solve.InProgress -> ()

let create_window board_ref ~key_press_handler ~click_handler =
  (* Create window *)
  let padding = 40 in
  let window =
    GWindow.window ~title:"Sudoku"
      ~width:(Configure_ui.total_size + padding)
      ~height:(Configure_ui.total_size + padding)
      ~resizable:true ()
  in

  let vbox = GPack.vbox ~packing:window#add () in

  (* Add menu *)
  Ui_menu.create_menu window board_ref vbox ;

  (* Handle window close *)
  let _ = window#connect#destroy ~callback:quit in

  (* Drawing area *)
  let drawing_area =
    GMisc.drawing_area ~width:Configure_ui.total_size ~height:Configure_ui.total_size
      ~packing:(vbox#pack ~expand:true ~fill:true)
      ()
  in

  (* Store drawing area reference for global access *)
  Ui_state.current_drawing_area := Some drawing_area ;
  Ui_state.current_board := !board_ref ;

  (* Handle window resize *)
  let _ =
    drawing_area#event#connect#configure ~callback:(fun _ ->
        GtkBase.Widget.queue_draw drawing_area#as_widget ;
        true)
  in

  (* Drawing callback *)
  let _ =
    drawing_area#event#connect#expose ~callback:(fun _ ->
        let ctxt = Cairo_gtk.create drawing_area#misc#window in
        (* Draw the main board *)
        Ui_board.draw_board ctxt !board_ref !Ui_state.selected ;
        (* Draw the hints if the show hints option is on *)
        if !Ui_state.show_hints then
          begin
            (* If the hints are empty, compute them *)
            if Array.for_all (Array.for_all (fun x -> x = [])) !Ui_state.current_hints then
              Ui_state.current_hints := Hints.compute_all_hints !board_ref;
            (* Draw the hints *)
            Hints.draw_hints ctxt !Ui_state.current_hints
          end;
        true)
  in

  (* Mouse click callback *)
  let _ =
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
  in

  (* Make sure drawing area can receive mouse events *)
  drawing_area#event#add [`BUTTON_PRESS] ;
  drawing_area#misc#set_can_focus true ;

  (* Key press callback - handles all keyboard input for the game *)
  let _ =
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
          match (!Ui_state.selected, key_press_handler key) with
          (* Case 1: Clear cell (key value is 0) *)
          | Some (row, col), Some 0 ->
              Ui_debug.debug "Attempting to clear cell at (%d,%d)\n" row col ;
              (match Solve.clear_cell !board_ref ~row ~col with
              | Some new_board ->
                  Ui_debug.debug "Cell cleared successfully\n" ;
                  (* Clear any invalid marking from the cell *)
                  Invalid_cells.clear_invalid ~row ~col ;
                  (* Update the board reference with the new state *)
                  board_ref := new_board ;
                  (* Reset hints since the board has changed *)
                  Ui_state.current_hints := Hints.clear_all_hints () ;
                  (* Check if the game is complete after clearing *)
                  check_game_completion !board_ref ;
                  (* Request a redraw of the drawing area *)
                  GtkBase.Widget.queue_draw drawing_area#as_widget
              | None -> Ui_debug.debug "Failed to clear cell\n") ;
              true
          (* Case 2: Set a value (key value is 1-9) *)
          | Some (row, col), Some value ->
              Ui_debug.debug "Attempting to set value %d at (%d,%d)\n" value row col ;
              (match Solve.set_cell !board_ref ~row ~col ~value with
              | Some (new_board, is_valid) ->
                  Ui_debug.debug "Cell updated successfully\n" ;
                  (* Update the board reference with the new state *)
                  board_ref := new_board ;
                  (* Reset hints since the board has changed *)
                  Ui_state.current_hints := Hints.clear_all_hints () ;
                  if is_valid
                  then (
                    (* If the move is valid, clear any invalid marking *)
                    Invalid_cells.clear_invalid ~row ~col ;
                    (* Check if the game is complete after the move *)
                    check_game_completion new_board)
                  else
                    (* If the move is invalid, mark the cell as invalid *)
                    Invalid_cells.mark_invalid ~row ~col ;
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
  in

  window

let start_ui ?(debug = false) ?(key_press_handler = fun _ -> None)
    ?(click_handler = fun _ _ -> ()) initial_board =
  Ui_debug.debug_mode := debug ;
  let _ = GMain.init () in
  let board = ref (Board.of_array initial_board) in
  Ui_state.reset_game_state ();
  let window = create_window board ~key_press_handler ~click_handler in
  window#show () ;
  GMain.main ()
