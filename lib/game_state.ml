(* Represents the current status of the game *)
type game_status =
  | InProgress
  | Complete of string (* Contains the completion message *)

(* Checks if the game has been successfully completed and returns the
   appropriate status. *)
let get_game_status board =
  let solved = Board_rules.is_board_solved board in
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
    | Complete message -> (
        Ui_debug.debug "Game completed: %s\n" message ;
        flush stdout ;
        let dialog =
          GWindow.message_dialog ~message ~message_type:`QUESTION
            ~buttons:GWindow.Buttons.yes_no ~title:"Game Complete" ()
        in
        Ui_debug.debug "Showing completion dialog\n" ;
        flush stdout ;
        dialog#show () ;
        let response = dialog#run () in
        Ui_debug.debug "Dialog response: %s\n"
          (match response with
          | `YES -> "YES"
          | `NO -> "NO"
          | `DELETE_EVENT -> "DELETE_EVENT") ;
        flush stdout ;
        dialog#destroy () ;
        Ui_debug.debug "Dialog closed\n" ;
        flush stdout ;
        (* Return true if user wants to start a new game, false otherwise *)
        match response with
        | `YES -> true
        | `NO | `DELETE_EVENT -> false)
    | InProgress ->
        Ui_debug.debug "Game in progress\n" ;
        flush stdout ;
        false
  with e ->
    Ui_debug.debug "Error in game completion check: %s\n" (Printexc.to_string e) ;
    flush stdout ;
    false
