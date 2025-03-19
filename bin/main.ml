open Sudoku_OCaml

let () =
  Random.self_init () ;
  let board =
    Generate_board.generate_random_board ~difficulty:Generate_board.Easy ()
  in

  let key_press_handler key =
    match key with
    | 65288 (* Backspace*) | 65535 (* Delete *) -> Some 0
    | key -> (
        match Char.chr key with
        | '1' .. '9' as c ->
            let value = int_of_string (String.make 1 c) in
            Some value
        | _ -> None)
  in

  let click_handler _x _y = Ui_state.refresh_display () in
  Ui_main.start_ui ~debug:true ~key_press_handler ~click_handler board
