open Soduku_OCaml

let () =
  Random.self_init () ;
  let board = Generate_board.generate_easy_board () in
  Ui.start_ui board
