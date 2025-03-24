open GWindow
open Ui_debug

(* UI-specific game completion handling *)

(* Shows a completion dialog and returns whether the user wants to start a new
   game *)
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
