let create_window board_ref ~key_press_handler ~click_handler =
  (* Create window *)
  let padding = 40 in
  let window =
    GWindow.window ~title:"Sudoku"
      ~width:(Ui_config.total_size + padding)
      ~height:(Ui_config.total_size + padding)
      ~resizable:true ()
  in

  let vbox = GPack.vbox ~packing:window#add () in

  (* Add menu *)
  Ui_menu.create_menu window board_ref vbox ;

  (* Handle window close *)
  let _ = window#connect#destroy ~callback:GMain.quit in

  (* Drawing area *)
  let drawing_area =
    GMisc.drawing_area ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  drawing_area#misc#set_size_request ~width:Ui_config.total_size
    ~height:Ui_config.total_size () ;

  (* Store drawing area reference for global access *)
  Ui_state.current_drawing_area := Some drawing_area ;
  Ui_state.current_board := !board_ref ;

  (* Connect to the draw signal using the underlying widget *)
  let draw_callback ctx =
    (* Draw the main board *)
    Ui_board.draw_board ctx !board_ref !Ui_state.selected ;
    (* Draw the hints if the show hints option is on *)
    if !Ui_state.show_hints
    then begin
      (* If the hints are empty, compute them *)
      if Array.for_all (Array.for_all (fun x -> x = [])) !Ui_state.current_hints
      then Ui_state.current_hints := Hints.get_all_hints !board_ref ;
      (* Draw the hints *)
      Ui_hints.draw_hints ctx !Ui_state.current_hints
    end ;
    false
  in
  ignore
    (GtkSignal.connect ~sgn:GtkBase.Widget.S.draw ~callback:draw_callback
       drawing_area#as_widget) ;

  (* Mouse click callback *)
  let _ = ignore (Ui_events.handle_mouse_click drawing_area click_handler) in

  (* Enable button press events *)
  drawing_area#event#add [`BUTTON_PRESS] ;
  drawing_area#misc#set_can_focus true ;

  (* Key press callback - handles all keyboard input for the game *)
  let _ =
    ignore
      (Ui_events.handle_key_press window board_ref drawing_area
         key_press_handler)
  in

  window

let start_ui ?(debug = false) ?(key_press_handler = fun _ -> None)
    ?(click_handler = fun _ _ -> ()) initial_board =
  Ui_debug.debug_mode := debug ;
  let _ = GMain.init () in
  let board = ref (Board.of_array initial_board) in
  Ui_state.reset_game_state () ;
  let window = create_window board ~key_press_handler ~click_handler in
  window#show () ;
  GMain.main ()
