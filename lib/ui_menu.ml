(* Creates application menu system with game controls and difficulty
   selection. *)
let create_menu window board_ref (vbox : GPack.box) =
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in

  (* Create game menu *)
  let factory = new GMenu.factory menubar in
  let game_menu = factory#add_submenu "Game" in
  let game_factory = new GMenu.factory game_menu in

  (* Create submenu items *)
  let new_game_menu = game_factory#add_submenu "New Game" in
  let new_game_factory = new GMenu.factory new_game_menu in

  (* Adds difficulty menu item with board generation and state management. *)
  let add_difficulty_item label difficulty =
    ignore
      (new_game_factory#add_item label ~callback:(fun () ->
           Ui_state.reset_game_state () ;
           let new_board =
             match Board_generate.generate_safe ~difficulty () with
             | Some board -> Board.of_array board
             | None ->
                 Printf.eprintf
                   "Warning: Board generation failed, using empty board\n" ;
                 Board.of_array (Array.make_matrix 9 9 0)
           in
           board_ref := new_board ;
           Ui_state.current_board := new_board ;
           GtkBase.Widget.queue_draw window#as_widget))
  in

  add_difficulty_item "Easy" Board_generate.Easy ;
  add_difficulty_item "Medium" Board_generate.Medium ;
  add_difficulty_item "Hard" Board_generate.Hard ;

  (* Add separator and hint toggle menu item with immediate visual feedback. *)
  ignore (game_factory#add_separator ()) ;
  ignore
    (game_factory#add_item "Show Hints" ~callback:(fun _menuitem ->
         Ui_state.show_hints := not !Ui_state.show_hints ;
         Ui_state.refresh_display ())) ;

  (* Standard application menu items for complete user experience. *)
  ignore (game_factory#add_separator ()) ;
  ignore (game_factory#add_item "Quit" ~callback:GMain.quit)
