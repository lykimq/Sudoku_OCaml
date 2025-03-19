(* The current board. *)
let current_board = ref (Board.create ())

(* The current drawing area. *)
let current_drawing_area : GMisc.drawing_area option ref = ref None

(* The currently selected cell on the board.  *)
let selected : (int * int) option ref = ref None

(* Whether to show hints on the board. *)
let show_hints: bool ref = ref false

(* The current hints on the board. *)
let current_hints: int list array array ref = ref (Hints.clear_all_hints ())

(* Updates the current board and resets the hints. *)
let update_board new_board =
  current_board := new_board ;
  current_hints := Hints.clear_all_hints ();
  match !current_drawing_area with
  | Some drawing_area -> GtkBase.Widget.queue_draw drawing_area#as_widget
  | None -> ()

(* Refreshes the display of the current drawing area. *)
let refresh_display () =
  match !current_drawing_area with
  | Some area -> GtkBase.Widget.queue_draw area#as_widget
  | None -> ()

(* Resets the game state to its initial state. *)
let reset_game_state () =
  selected := None;
  show_hints := false;
  current_hints := Hints.clear_all_hints ();
  Ui_debug.debug "Game state reset: hints cleared and disabled, selection removed"