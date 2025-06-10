open Sudoku_OCaml

(** Main application initialization and startup sequence.

    Algorithm: Application bootstrap sequence
    - Initialize random number generator for deterministic testing/debugging
    - Generate initial game board using default difficulty
    - Configure keyboard input mapping for multi-platform compatibility
    - Configure mouse interaction handler for UI refresh coordination
    - Start UI with debug mode enabled and configured handlers *)
let () =
  Random.self_init () ;
  let board = Game_state.create_new_board () in
  let key_press_handler key =
    match key with
    | 65288 (* Backspace*) | 65535 (* Delete *) | 65456 (* KP_0 *) -> Some 0
    (* Numpad keys: use range matching for cleaner code *)
    | n when n >= 65457 && n <= 65465 -> Some (n - 65456) (* KP_1 to KP_9 *)
    (* Regular ASCII digit keys *)
    | n when n >= 49 && n <= 57 -> Some (n - 48) (* '1' to '9' *)
    | _ -> None
  in
  let click_handler _x _y = Ui_state.refresh_display () in
  Ui_main.start_ui ~debug:true ~key_press_handler ~click_handler board
