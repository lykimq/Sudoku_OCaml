(* Represents the current status of the game *)
type game_status =
  | InProgress
  | Complete of string (* Contains the completion message *)

(* Checks if the game has been successfully completed and returns the
   appropriate status. *)
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
