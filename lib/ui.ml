open GMain

let create_menu window board_ref (vbox : GPack.box) =
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in

  (* Create game menu *)
  let factory = new GMenu.factory menubar in
  let game_menu = factory#add_submenu "Game" in
  let game_factory = new GMenu.factory game_menu in

  (* Create submenu items *)
  let new_game_menu = game_factory#add_submenu "New Game" in
  let new_game_factory = new GMenu.factory new_game_menu in

  (* Add difficulty options *)
  let add_difficulty_item label difficulty =
    ignore
      (new_game_factory#add_item label ~callback:(fun () ->
           board_ref :=
             Board.of_array
               (Generate_board.generate_random_board ~difficulty ()) ;
           GtkBase.Widget.queue_draw window#as_widget))
  in

  add_difficulty_item "Easy" Generate_board.Easy ;
  add_difficulty_item "Medium" Generate_board.Medium ;
  add_difficulty_item "Hard" Generate_board.Hard ;

  (* Add menu items to game menu *)
  ignore (game_factory#add_separator ()) ;
  ignore (game_factory#add_item "Quit" ~callback:quit)

let create_window board_ref =
  (* Create window *)
  let padding = 40 in
  let window =
    GWindow.window ~title:"Sudoku"
      ~width:(Board.total_size + padding)
      ~height:(Board.total_size + padding)
      ~resizable:true ()
  in

  let vbox = GPack.vbox ~packing:window#add () in

  (* Add menu *)
  create_menu window board_ref vbox ;

  (* Handle window close *)
  let _ = window#connect#destroy ~callback:quit in

  (* Drawing area *)
  let drawing_area =
    GMisc.drawing_area ~width:Board.total_size ~height:Board.total_size
      ~packing:(vbox#pack ~expand:true ~fill:true)
      ()
  in

  let _ =
    drawing_area#event#connect#configure ~callback:(fun _ ->
        GtkBase.Widget.queue_draw drawing_area#as_widget ;
        true)
  in

  let selected = ref None in

  (* Drawing callback *)
  let _ =
    drawing_area#event#connect#expose ~callback:(fun _ ->
        let cr = Cairo_gtk.create drawing_area#misc#window in
        Board.draw_board cr !board_ref !selected ;
        true)
  in

  (* Mouse click callback *)
  let _ =
    drawing_area#event#connect#button_press ~callback:(fun ev ->
        let x = int_of_float (GdkEvent.Button.x ev) in
        let y = int_of_float (GdkEvent.Button.y ev) in
        selected := Board.screen_to_board_pos x y ;
        GtkBase.Widget.queue_draw drawing_area#as_widget ;
        true)
  in

  (* Key press callback *)
  let _ =
    window#event#connect#key_press ~callback:(fun ev ->
        match (GdkEvent.Key.keyval ev, !selected) with
        | k, Some (row, col) when k >= 49 && k <= 57 ->
            let value = k - 48 in
            (match Board.set_cell !board_ref ~row ~col ~value with
            | Some new_board ->
                board_ref := new_board ;
                GtkBase.Widget.queue_draw drawing_area#as_widget
            | None -> ()) ;
            true
        | _, _ -> false)
  in

  window

let start_ui initial_board =
  let _ = GMain.init () in
  let board = ref (Board.of_array initial_board) in
  let window = create_window board in
  window#show () ;
  GMain.main ()
