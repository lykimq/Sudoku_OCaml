(* Creates main application window with complete UI setup and event handling.

   Functional Purpose: Orchestrates the creation of the complete user interface
   including window, menu, drawing area, and all event handlers.

   Design Choice: Single window creation function over modular construction -
   Centralized UI setup for consistent initialization order - Dependency
   management between UI components handled automatically - Complete window
   configuration in one place for maintainability - Integration of all UI
   subsystems (menu, drawing, events)

   Algorithm: Sequential UI component construction with dependency management 1.
   Create main window with calculated dimensions and properties 2. Create
   vertical layout container for menu and drawing area 3. Add menu system with
   game controls and difficulty selection 4. Set up window lifecycle management
   (close event handling) 5. Create and configure drawing area with size
   constraints 6. Register global state references for cross-component
   communication 7. Set up drawing callback with lazy hint computation 8.
   Configure event handlers for mouse and keyboard interaction 9. Enable
   necessary events and focus management Time: O(1) for UI setup +
   O(callback_registration)

   Window sizing strategy: - Base size: Calculated from board dimensions and
   cell size - Padding: Additional space for menu bar and window decorations -
   Resizable: Allows user customization while maintaining aspect ratio

   Layout management: - VBox container: Vertical stacking of menu and drawing
   area - Menu packing: Automatic sizing at top of window - Drawing area:
   Expandable to fill remaining space - Size constraints: Minimum size ensures
   usability *)
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

  (* Main draw callback with lazy hint computation and layered rendering.

     Functional Purpose: Renders complete game visualization with optional hints
     overlay using efficient caching and lazy evaluation.

     Design Choice: Lazy hint computation over eager calculation - Hints only
     computed when display mode is enabled - Cache validation prevents redundant
     expensive calculations - Layered rendering: board first, then hints overlay

     - Consistent board reference usage across rendering components

     Algorithm: Conditional rendering with cache management 1. Render main board
     with current state and selection 2. If hints enabled: - Check cache
     validity (empty arrays indicate not computed) - Compute hints if cache
     invalid (expensive operation) - Render hint overlay on top of board 3.
     Return false to indicate drawing complete Time: O(board_rendering) +
     O(hint_computation) if cache miss

     Performance optimizations: - Lazy hint computation: Only when needed and
     cache invalid - Cache validation: Array.for_all check is faster than
     recomputation - Layered rendering: Hints overlay doesn't require board
     re-render - Consistent state: Single board reference prevents
     inconsistencies

     Alternative: Could use incremental hint updates or separate hint window *)
  let draw_callback ctx =
    (* Use consistent board reference for both board drawing and hints *)
    Ui_board.draw_board ctx !Ui_state.current_board !Ui_state.selected ;
    if !Ui_state.show_hints
    then begin
      (* Lazy hint computation - only calculate when needed. Check if hints are
         already computed (all arrays are empty), if not, compute them. *)
      if Array.for_all (Array.for_all (fun x -> x = [])) !Ui_state.current_hints
      then Ui_state.current_hints := Hints.get_all_hints !Ui_state.current_board ;
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

(* Main UI entry point with configurable handlers and debug mode.

   Functional Purpose: Provides the primary interface for starting the Sudoku
   application with customizable behavior and initialization.

   Design Choice: Functional approach with optional parameters - Default
   parameters for simple usage (start_ui board) - Optional customization for
   advanced usage and testing - Debug mode control for development vs production
   - Clean separation between UI setup and game logic

   Algorithm: Application initialization and main loop startup 1. Configure
   debug mode for development visibility 2. Initialize GTK+ windowing system 3.
   Create board reference from initial board data 4. Reset game state to clean
   initial conditions 5. Create main window with all UI components 6. Show
   window and start GTK+ main event loop Time: O(UI_initialization) +
   O(main_loop) (runs until quit)

   Parameter design: - debug: Optional development mode for troubleshooting -
   key_press_handler: Customizable keyboard input processing - click_handler:
   Customizable mouse interaction handling - initial_board: Starting game state
   (typically generated puzzle)

   State management: - Board reference: Mutable reference for game state updates
   - UI state reset: Clean slate for new application instance - GTK+
   initialization: Required for all windowing operations

   Alternative approaches: - Configuration object: More parameters but more
   complex - Builder pattern: More flexible but heavier API - Separate
   initialization: More control but more complex usage *)
let start_ui ?(debug = false) ?(key_press_handler = fun _ -> None)
    ?(click_handler = fun _ _ -> ()) initial_board =
  Ui_debug.debug_mode := debug ;
  let _ = GMain.init () in
  let board = ref (Board.of_array initial_board) in
  Ui_state.reset_game_state () ;
  let window = create_window board ~key_press_handler ~click_handler in
  window#show () ;
  GMain.main ()
