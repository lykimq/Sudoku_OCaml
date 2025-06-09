let create_window board_ref ~key_press_handler ~click_handler =
  (* Create main window with padding for UI elements *)
  let padding = 40 in
  let window =
    GWindow.window ~title:"Sudoku"
      ~width:(Ui_config.total_size + padding)
      ~height:(Ui_config.total_size + padding)
      ~resizable:true ()
  in

  let vbox = GPack.vbox ~packing:window#add () in

  (* Add menu bar *)
  Ui_menu.create_menu window board_ref vbox ;

  (* Handle window close event *)
  let _ = window#connect#destroy ~callback:GMain.quit in

  (* Create drawing area for game board *)
  let drawing_area =
    GMisc.drawing_area ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  drawing_area#misc#set_size_request ~width:Ui_config.total_size
    ~height:Ui_config.total_size () ;

  (* Store references for global access *)
  Ui_state.current_drawing_area := Some drawing_area ;
  Ui_state.current_board := !board_ref ;

  (* Main draw callback - renders board and optional hints *)
  let draw_callback ctx =
    Ui_board.draw_board ctx !board_ref !Ui_state.selected ;
    if !Ui_state.show_hints
    then begin
      (* Lazy hint computation - only calculate when needed *)
      if Array.for_all (Array.for_all (fun x -> x = [])) !Ui_state.current_hints
      then Ui_state.current_hints := Hints.get_all_hints !board_ref ;
      Ui_hints.draw_hints ctx !Ui_state.current_hints
    end ;
    false
  in
  ignore
    (GtkSignal.connect ~sgn:GtkBase.Widget.S.draw ~callback:draw_callback
       drawing_area#as_widget) ;

  (* Event handlers *)
  let _ = ignore (Ui_events.handle_mouse_click drawing_area click_handler) in

  (* Enable events and focus *)
  drawing_area#event#add [`BUTTON_PRESS] ;
  drawing_area#misc#set_can_focus true ;

  let _ =
    ignore
      (Ui_events.handle_key_press window board_ref drawing_area
         key_press_handler)
  in

  window

(** Main UI entry point with configurable handlers and debug mode.

    Design: Functional approach with optional parameters for customization.
    State initialization: Resets game state and creates initial board reference.
*)
let start_ui ?(debug = false) ?(key_press_handler = fun _ -> None)
    ?(click_handler = fun _ _ -> ()) initial_board =
  Ui_debug.debug_mode := debug ;
  let _ = GMain.init () in
  let board = ref (Board.of_array initial_board) in
  Ui_state.reset_game_state () ;
  let window = create_window board ~key_press_handler ~click_handler in
  window#show () ;
  GMain.main ()
