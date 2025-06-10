open GWindow
open Ui_debug

(* Shows modal completion dialog and handles user response for new game flow.

   Functional Purpose: Provides user interaction for game completion with clear
   options and immediate feedback for continuing or ending gameplay.

   Design Choice: Modal dialog over non-modal notification - Ensures user
   acknowledgment of completion - Prevents accidental game state changes during
   decision - Clear binary choice (new game vs quit) simplifies UX - Blocks game
   interaction until decision made

   Algorithm: GTK+ dialog lifecycle management 1. Create modal message dialog
   with question type 2. Configure yes/no buttons for binary choice 3. Show
   dialog and wait for user response (blocking) 4. Pattern match response for
   type-safe handling 5. Clean up dialog resources immediately 6. Return boolean
   indicating user choice

   UI/UX Design Decisions: - Question dialog type: Indicates user choice
   required - Yes/No buttons: Clear, standard interface pattern - Modal
   behavior: Prevents confusion with multiple dialogs - Immediate cleanup:
   Prevents resource leaks

   Error Handling: Graceful handling of dialog system failures - DELETE_EVENT
   treated as "No" (user closed dialog) - Debug logging for troubleshooting UI
   issues - Resource cleanup guaranteed via immediate destroy

   Alternative approaches: - Non-modal notification: Less intrusive but easier
   to miss - Custom dialog: More control but more complex implementation -
   In-game overlay: Modern approach but requires more UI work - Auto-restart:
   Simpler but removes user choice *)
let show_completion_dialog message =
  debug "Showing completion dialog\n" ;
  flush stdout ;

  let dialog =
    message_dialog ~message ~message_type:`QUESTION ~buttons:Buttons.yes_no
      ~title:"Game Complete" ()
  in

  dialog#show () ;
  let response = dialog#run () in

  debug "Dialog response: %s\n"
    (match response with
    | `YES -> "YES"
    | `NO -> "NO"
    | `DELETE_EVENT -> "DELETE_EVENT") ;
  flush stdout ;

  dialog#destroy () ;
  debug "Dialog closed\n" ;
  flush stdout ;

  (* Return true if user wants to start a new game, false otherwise *)
  match response with
  | `YES -> true
  | `NO | `DELETE_EVENT -> false
