(* Global UI state management for the Sudoku game. *)

(* Current board state reference for game logic and display. *)
let current_board = ref (Board.create ())

(* Current drawing area widget reference for rendering operations. *)
let current_drawing_area : GMisc.drawing_area option ref = ref None

(* Currently selected cell coordinates for user interaction feedback. *)
let selected : (int * int) option ref = ref None

(* Toggle for showing/hiding hints on the board. *)
let show_hints : bool ref = ref false

(* Cached hints for all empty cells to avoid recomputation. *)
let current_hints : int list array array ref =
  ref (Hints.make_empty_hint_board ())

(* Updates board and resets hints, triggering redraw if drawing area
   available. *)

let update_board new_board =
  current_board := new_board ;
  current_hints := Hints.make_empty_hint_board () ;
  match !current_drawing_area with
  | Some drawing_area ->
      (* Schedule a redraw instead of drawing immediately - more efficient and
         prevents screen flickering from multiple rapid updates *)
      GtkBase.Widget.queue_draw drawing_area#as_widget
  | None -> ()

(* Triggers redraw of current drawing area if available. *)
let refresh_display () =
  match !current_drawing_area with
  | Some area ->
      (* Queue the refresh instead of immediate draw - GTK batches multiple
         requests together for smoother performance *)
      GtkBase.Widget.queue_draw area#as_widget
  | None -> ()

(* Resource cleanup function to prevent memory leaks. *)
let cleanup_resources () =
  (* Don't clear current_drawing_area as it's needed for the game to function *)
  current_hints := Hints.make_empty_hint_board () ;
  Gc.minor () (* Trigger minor garbage collection *)

(* Resets all game state to initial values for new game. *)
let reset_game_state () =
  cleanup_resources () ;
  selected := None ;
  show_hints := false ;
  Ui_debug.debug "Game state reset with resource cleanup"
