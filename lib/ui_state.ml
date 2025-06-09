(** Global UI state management for the Sudoku game. *)

(** Current board state. *)
let current_board = ref (Board.create ())

(** Current drawing area widget reference. *)
let current_drawing_area : GMisc.drawing_area option ref = ref None

(** Currently selected cell coordinates (row, col). *)
let selected : (int * int) option ref = ref None

(** Toggle for showing/hiding hints on the board. *)
let show_hints : bool ref = ref false

(** Cached hints for all empty cells to avoid recomputation. *)
let current_hints : int list array array ref =
  ref (Hints.make_empty_hint_board ())

(** Updates board and resets hints, triggering redraw if drawing area available.
*)
let update_board new_board =
  current_board := new_board ;
  current_hints := Hints.make_empty_hint_board () ;
  match !current_drawing_area with
  | Some drawing_area -> GtkBase.Widget.queue_draw drawing_area#as_widget
  | None -> ()

(** Triggers redraw of current drawing area if available. *)
let refresh_display () =
  match !current_drawing_area with
  | Some area -> GtkBase.Widget.queue_draw area#as_widget
  | None -> ()

(** Resets all game state to initial values for new game. *)
let reset_game_state () =
  selected := None ;
  show_hints := false ;
  current_hints := Hints.make_empty_hint_board () ;
  Ui_debug.debug
    "Game state reset: hints cleared and disabled, selection removed"
