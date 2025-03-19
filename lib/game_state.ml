(* Represents the current status of the game *)
type game_status =
  | InProgress
  | Complete of string (* Contains the completion message *)

(* Checks if the game has been successfully completed and returns the
   appropriate status. *)
let get_game_status board =
  if Rules_board.is_board_solved board
  then
    Complete
      "Congratulations! You've solved the Sudoku puzzle correctly! Would you \
       like to start a new game?"
  else InProgress
