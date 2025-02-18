open GMain

let debug_mode = ref false

let debug fmt =
  if !debug_mode
  then
    Printf.kprintf
      (fun s ->
        Printf.printf "%s\n" s ;
        flush stdout)
      fmt
  else Printf.kprintf (fun _ -> ()) fmt

let current_board = ref (Board.create ())
let current_drawing_area = ref None
let selected = ref None

let update_board new_board =
  current_board := new_board ;
  match !current_drawing_area with
  | Some drawing_area -> GtkBase.Widget.queue_draw drawing_area#as_widget
  | None -> ()

let refresh_display () =
  match !current_drawing_area with
  | Some area -> GtkBase.Widget.queue_draw area#as_widget
  | None -> ()

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

let create_window board_ref ~key_press_handler ~click_handler =
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

  (* Store drawing area reference for global access *)
  current_drawing_area := Some drawing_area ;
  current_board := !board_ref ;

  let _ =
    drawing_area#event#connect#configure ~callback:(fun _ ->
        GtkBase.Widget.queue_draw drawing_area#as_widget ;
        true)
  in

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
        debug "Raw click at (x=%d, y=%d)\n" x y ;

        (* Add event button check *)
        let button = GdkEvent.Button.button ev in
        debug "Mouse button: %d\n" button ;

        (* Make sure we're only handling left clicks *)
        if button = 1
        then begin
          let pos = Board.screen_to_board_pos x y in
          debug "Converting to board position: %s\n"
            (match pos with
            | Some (row, col) -> Printf.sprintf "(%d,%d)" row col
            | None -> "None") ;

          selected := pos ;
          debug "Updated selected cell to: %s\n"
            (match !selected with
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

  (* Key press callback *)
  let _ =
    window#event#connect#key_press ~callback:(fun ev ->
        let key = GdkEvent.Key.keyval ev in
        debug "Key pressed: %d\n" key ;
        debug "Current selection: %s\n"
          (match !selected with
          | Some (row, col) -> Printf.sprintf "(%d,%d)" row col
          | None -> "None") ;
        let result =
          match (!selected, key_press_handler key) with
          | Some (row, col), Some value ->
              debug "Attempting to set value %d at (%d,%d)\n" value row col ;
              (match Solve.set_cell !board_ref ~row ~col ~value with
              | Some new_board ->
                  debug "Cell updated successfully\n" ;
                  board_ref := new_board ;
                  GtkBase.Widget.queue_draw drawing_area#as_widget
              | None -> debug "Failed to update cell\n") ;
              true
          | _, _ ->
              debug "No valid selection or key handler result\n" ;
              false
        in
        flush stdout ;
        result)
  in

  window

let start_ui ?(debug = false) ?(key_press_handler = fun _ -> None)
    ?(click_handler = fun _ _ -> ()) initial_board =
  debug_mode := debug ;
  let _ = GMain.init () in
  let board = ref (Board.of_array initial_board) in
  let window = create_window board ~key_press_handler ~click_handler in
  window#show () ;
  GMain.main ()
