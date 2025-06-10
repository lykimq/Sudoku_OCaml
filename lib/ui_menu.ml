(* Creates application menu system with game controls and difficulty selection.

   Functional Purpose: Provides hierarchical menu interface for game management,
   difficulty selection, and user preferences.

   Design Choice: GTK+ native menu system over custom UI controls -
   Platform-consistent appearance and behavior - Keyboard navigation and
   accessibility support built-in - Standard menu patterns familiar to users -
   Automatic layout and styling management

   Algorithm: Hierarchical menu construction with callback registration 1.
   Create menu bar container and attach to window layout 2. Build menu
   hierarchy: Game -> New Game -> Difficulty options 3. Register callbacks for
   each menu item with appropriate actions 4. Configure menu item properties
   (labels, separators, etc.) Time: O(menu_items) = O(1) for fixed menu
   structure

   Menu structure rationale: - Game menu: Primary game actions (standard
   application pattern) - New Game submenu: Groups difficulty options logically
   - Difficulty items: Easy/Medium/Hard progression - Hint toggle: Immediate
   user assistance control - Quit option: Standard application exit

   Alternative: Could use toolbar, ribbon interface, or context menus *)
let create_menu window board_ref (vbox : GPack.box) =
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in

  (* Create game menu *)
  let factory = new GMenu.factory menubar in
  let game_menu = factory#add_submenu "Game" in
  let game_factory = new GMenu.factory game_menu in

  (* Create submenu items *)
  let new_game_menu = game_factory#add_submenu "New Game" in
  let new_game_factory = new GMenu.factory new_game_menu in

  (* Adds difficulty menu item with board generation and state management.

     Functional Purpose: Creates menu item that triggers complete game restart
     with specified difficulty level.

     Design Choice: Higher-order function for DRY principle - Eliminates code
     duplication across difficulty levels - Consistent behavior for all
     difficulty options - Easy addition of new difficulty levels - Centralized
     error handling for board generation

     Algorithm: Menu item creation with callback registration 1. Create menu
     item with specified label 2. Register callback that: - Resets all game
     state to clean slate - Generates new board with specified difficulty -
     Handles generation failure gracefully with fallback - Updates both local
     and global board references - Triggers UI redraw for immediate visual
     feedback Time: O(1) for menu creation + O(board_generation) for callback

     Error handling: Graceful fallback for generation failures - Logs warning
     for debugging purposes - Provides empty playable board as fallback -
     Ensures game remains functional even with generation issues

     State management: Comprehensive state reset and update -
     Ui_state.reset_game_state(): Clears selection, hints, caches - Board
     reference updates: Both local and global consistency - UI redraw: Immediate
     visual feedback for user

     Alternative: Could use separate functions per difficulty or
     configuration-driven menu generation *)
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

  (* Add separator and hint toggle menu item with immediate visual feedback.

     Functional Purpose: Provides user control over hint display mode with
     immediate visual feedback and state persistence.

     Design Choice: Toggle behavior over separate show/hide items - Single menu
     item with state-aware behavior - Immediate toggle without confirmation
     dialogs - State persists until explicitly changed by user - Visual feedback
     through immediate redraw

     Algorithm: State toggle with UI refresh 1. Toggle boolean hint display flag
     2. Trigger immediate UI refresh to show/hide hints Time: O(1) for state
     change + O(UI_refresh) for visual update

     Alternative: Could use checkbox menu item, separate show/hide items, or
     keyboard shortcut only *)
  ignore (game_factory#add_separator ()) ;
  ignore
    (game_factory#add_item "Show Hints" ~callback:(fun _menuitem ->
         Ui_state.show_hints := not !Ui_state.show_hints ;
         Ui_state.refresh_display ())) ;

  (* Standard application menu items for complete user experience.

     Functional Purpose: Provides expected application lifecycle controls
     following standard desktop application conventions.

     Design Choice: Standard menu items over custom controls - Separator: Visual
     grouping of related vs unrelated actions - Quit item: Standard application
     exit following conventions - GMain.quit callback: Proper GTK+ application
     termination

     Alternative: Could use window close button only or custom exit handling *)
  ignore (game_factory#add_separator ()) ;
  ignore (game_factory#add_item "Quit" ~callback:GMain.quit)
