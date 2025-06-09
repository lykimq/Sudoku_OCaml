(** Game status tracking for completion detection. *)
type game_status =
  | InProgress
  | Complete of string (* Completion message *)

(** Checks board completion and returns appropriate status with message. *)
let get_game_status board =
  let solved = Board_validation.is_board_solved board in
  Ui_debug.debug "Solved: %b\n" solved ;
  flush stdout ;

  if solved
  then
    Complete
      "Congratulations! You've solved the Sudoku puzzle correctly! Would you \
       like to start a new game?"
  else InProgress

(** Checks for game completion and shows dialog if solved. Returns true if new
    game was started, false otherwise. *)
let check_game_completion board =
  Ui_debug.debug "Checking game completion\n" ;
  flush stdout ;

  try
    match get_game_status board with
    | Complete message ->
        Ui_debug.debug "Game completed: %s\n" message ;
        flush stdout ;
        Ui_game_completion.show_completion_dialog message
    | InProgress ->
        Ui_debug.debug "Game in progress\n" ;
        flush stdout ;
        false
  with e ->
    Ui_debug.debug "Error in game completion check: %s\n" (Printexc.to_string e) ;
    flush stdout ;
    false
