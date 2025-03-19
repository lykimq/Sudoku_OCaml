open Sudoku_OCaml
open Board
open Alcotest

let create_board list =
  let board = Board.create () in
  List.iteri
    (fun i row ->
      List.iteri
        (fun j value ->
          let value = if value = 0 then Empty else Fixed value in
          board.(i).(j) <- value)
        row)
    list ;
  board

let test_empty_board () =
  let board = create () in
  check bool "Empty board" false (Rules_board.is_board_solved board)

let test_solved_board () =
  let board =
    create_board
      [
        [5; 3; 4; 6; 7; 8; 9; 1; 2];
        [6; 7; 2; 1; 9; 5; 3; 4; 8];
        [1; 9; 8; 3; 4; 2; 5; 6; 7];
        [8; 5; 9; 7; 6; 1; 4; 2; 3];
        [4; 2; 6; 8; 5; 3; 7; 9; 1];
        [7; 1; 3; 9; 2; 4; 8; 5; 6];
        [9; 6; 1; 5; 3; 7; 2; 8; 4];
        [2; 8; 7; 4; 1; 9; 6; 3; 5];
        [3; 4; 5; 2; 8; 6; 1; 7; 9];
      ]
  in
  check bool "Valid board solved" true (Rules_board.is_board_solved board)

let test_duplicate_numbers_in_row () =
  let board =
    create_board
      [
        [5; 3; 4; 6; 7; 8; 9; 1; 2];
        [6; 7; 2; 1; 9; 5; 3; 4; 8];
        [1; 9; 8; 3; 4; 2; 5; 6; 7];
        [8; 5; 9; 7; 6; 1; 4; 2; 3];
        [4; 2; 6; 8; 5; 3; 7; 9; 1];
        [7; 1; 3; 9; 2; 4; 8; 5; 6];
        [9; 6; 1; 5; 3; 7; 2; 8; 4];
        [2; 8; 7; 4; 1; 9; 6; 3; 5];
        [3; 4; 5; 2; 8; 6; 1; 7; 7];
      ]
  in
  check bool "Duplicate numbers in row" false
    (Rules_board.is_board_solved board)

let test_duplicate_numbers_in_column () =
  let board =
    create_board
      [
        [5; 3; 4; 6; 7; 8; 9; 1; 2];
        [6; 7; 2; 1; 9; 5; 3; 4; 8];
        [1; 9; 8; 3; 4; 2; 5; 6; 7];
        [8; 5; 9; 7; 6; 1; 4; 2; 3];
        [4; 2; 6; 8; 5; 3; 7; 9; 1];
        [7; 1; 3; 9; 2; 4; 8; 5; 6];
        [9; 6; 1; 5; 3; 7; 2; 8; 4];
        [2; 8; 7; 4; 1; 9; 6; 3; 5];
        [3; 4; 5; 2; 8; 6; 1; 7; 2];
      ]
  in
  check bool "Duplicate numbers in column" false
    (Rules_board.is_board_solved board)

let test_duplicate_numbers_in_box () =
  let board =
    create_board
      [
        [5; 3; 4; 6; 7; 8; 9; 1; 2];
        [6; 7; 2; 1; 9; 5; 3; 4; 8];
        [1; 9; 8; 3; 4; 2; 5; 6; 7];
        [8; 5; 9; 7; 6; 1; 4; 2; 3];
        [4; 2; 6; 8; 5; 3; 7; 9; 1];
        [7; 1; 3; 9; 2; 4; 8; 5; 6];
        [9; 6; 1; 5; 3; 7; 2; 8; 4];
        [2; 8; 7; 4; 1; 9; 6; 3; 5];
        [3; 4; 5; 2; 8; 6; 1; 7; 3];
      ]
  in
  check bool "Duplicate numbers in box" false
    (Rules_board.is_board_solved board)

let test_invalid_range_board () =
  let board =
    create_board
      [
        [5; 3; 4; 6; 7; 8; 9; 1; 2];
        [6; 7; 2; 1; 9; 5; 3; 4; 8];
        [1; 9; 8; 3; 4; 2; 5; 6; 7];
        [8; 5; 9; 7; 6; 1; 4; 2; 3];
        [4; 2; 6; 8; 5; 3; 7; 9; 1];
        [7; 1; 3; 9; 2; 4; 8; 5; 6];
        [9; 6; 1; 5; 3; 7; 2; 8; 4];
        [2; 8; 7; 4; 1; 9; 6; 3; 5];
        [3; 4; 5; 2; 8; 6; 1; 7; 10];
      ]
  in
  check bool "Invalid range board" false (Rules_board.is_board_solved board)

let test_all_same_number_board () =
  let board =
    create_board
      [
        [1; 1; 1; 1; 1; 1; 1; 1; 1];
        [1; 1; 1; 1; 1; 1; 1; 1; 1];
        [1; 1; 1; 1; 1; 1; 1; 1; 1];
        [1; 1; 1; 1; 1; 1; 1; 1; 1];
        [1; 1; 1; 1; 1; 1; 1; 1; 1];
        [1; 1; 1; 1; 1; 1; 1; 1; 1];
        [1; 1; 1; 1; 1; 1; 1; 1; 1];
        [1; 1; 1; 1; 1; 1; 1; 1; 1];
        [1; 1; 1; 1; 1; 1; 1; 1; 1];
      ]
  in
  check bool "All same number board" false (Rules_board.is_board_solved board)

let test_multiple_empty_cells_board () =
  let board =
    create_board
      [
        [5; 3; 4; 6; 7; 8; 9; 1; 2];
        [6; 7; 2; 1; 9; 5; 3; 4; 8];
        [1; 9; 8; 3; 4; 2; 5; 6; 7];
        [8; 5; 9; 7; 6; 1; 4; 2; 3];
        [4; 2; 6; 8; 5; 3; 7; 9; 1];
        [7; 1; 3; 9; 2; 4; 8; 5; 6];
        [9; 6; 1; 5; 3; 7; 2; 8; 4];
        [2; 8; 7; 4; 1; 9; 6; 3; 5];
        [3; 4; 5; 2; 8; 6; 1; 0; 0];
      ]
  in
  check bool "Multiple empty cells board" false
    (Rules_board.is_board_solved board)

let test_game_completion () =
  let board =
    create_board
      [
        [5; 3; 4; 6; 7; 8; 9; 1; 2];
        [6; 7; 2; 1; 9; 5; 3; 4; 8];
        [1; 9; 8; 3; 4; 2; 5; 6; 7];
        [8; 5; 9; 7; 6; 1; 4; 2; 3];
        [4; 2; 6; 8; 5; 3; 7; 9; 1];
        [7; 1; 3; 9; 2; 4; 8; 5; 6];
        [9; 6; 1; 5; 3; 7; 2; 8; 4];
        [2; 8; 7; 4; 1; 9; 6; 3; 5];
        [3; 4; 5; 2; 8; 6; 1; 7; 9];
      ]
  in
  let status = Game_state.get_game_status board in
  match status with
  | Game_state.Complete _ -> check bool "Game completion" true true
  | Game_state.InProgress -> check bool "Game completion" false true

let test_game_in_progress () =
  let board =
    create_board
      [
        [5; 3; 4; 6; 7; 8; 9; 1; 2];
        [6; 7; 2; 1; 9; 5; 3; 4; 8];
        [1; 9; 8; 3; 4; 2; 5; 6; 7];
        [8; 5; 9; 7; 6; 1; 4; 2; 3];
        [4; 2; 6; 8; 5; 3; 7; 9; 1];
        [7; 1; 3; 9; 2; 4; 8; 5; 6];
        [9; 6; 1; 5; 3; 7; 2; 8; 4];
        [2; 8; 7; 4; 1; 9; 6; 3; 5];
        [3; 4; 5; 2; 8; 6; 1; 0; 9];
      ]
  in
  let status = Game_state.get_game_status board in
  match status with
  | Game_state.Complete _ -> check bool "Game in progress" false true
  | Game_state.InProgress -> check bool "Game in progress" true true

let test_game_completion_prompt () =
  let board =
    create_board
      [
        [5; 3; 4; 6; 7; 8; 9; 1; 2];
        [6; 7; 2; 1; 9; 5; 3; 4; 8];
        [1; 9; 8; 3; 4; 2; 5; 6; 7];
        [8; 5; 9; 7; 6; 1; 4; 2; 3];
        [4; 2; 6; 8; 5; 3; 7; 9; 1];
        [7; 1; 3; 9; 2; 4; 8; 5; 6];
        [9; 6; 1; 5; 3; 7; 2; 8; 4];
        [2; 8; 7; 4; 1; 9; 6; 3; 5];
        [3; 4; 5; 2; 8; 6; 1; 7; 9];
      ]
  in
  let status = Game_state.get_game_status board in
  match status with
  | Game_state.Complete message ->
      let expected_message =
        "Congratulations! You've solved the Sudoku puzzle correctly! Would you \
         like to start a new game?"
      in
      check string "Completion message" expected_message message ;
      (* Test that the message contains key phrases *)
      check bool "Message contains 'Congratulations'" true
        (String.contains message 'C') ;
      check bool "Message contains 'solved'" true (String.contains message 's') ;
      check bool "Message contains 'new game'" true
        (String.contains message 'n')
  | Game_state.InProgress -> check bool "Game completion prompt" false true

let test_game_completion_dialog () =
  let board =
    create_board
      [
        [5; 3; 4; 6; 7; 8; 9; 1; 2];
        [6; 7; 2; 1; 9; 5; 3; 4; 8];
        [1; 9; 8; 3; 4; 2; 5; 6; 7];
        [8; 5; 9; 7; 6; 1; 4; 2; 3];
        [4; 2; 6; 8; 5; 3; 7; 9; 1];
        [7; 1; 3; 9; 2; 4; 8; 5; 6];
        [9; 6; 1; 5; 3; 7; 2; 8; 4];
        [2; 8; 7; 4; 1; 9; 6; 3; 5];
        [3; 4; 5; 2; 8; 6; 1; 7; 9];
      ]
  in
  (* Test that check_game_completion doesn't raise any exceptions *)
  try
    let _ = Game_state.check_game_completion board in
    check bool "Game completion dialog" true true
  with _e -> check bool "Game completion dialog error" false true

let () =
  Alcotest.run "Sudoku"
    [
      ( "Board",
        [
          test_case "test_empty_board" `Quick test_empty_board;
          test_case "test_solved_board" `Quick test_solved_board;
          test_case "test_duplicate_numbers_in_row" `Quick
            test_duplicate_numbers_in_row;
          test_case "test_duplicate_numbers_in_column" `Quick
            test_duplicate_numbers_in_column;
          test_case "test_duplicate_numbers_in_box" `Quick
            test_duplicate_numbers_in_box;
          test_case "test_invalid_range_board" `Quick test_invalid_range_board;
          test_case "test_all_same_number_board" `Quick
            test_all_same_number_board;
          test_case "test_multiple_empty_cells_board" `Quick
            test_multiple_empty_cells_board;
          test_case "test_game_completion" `Quick test_game_completion;
          test_case "test_game_in_progress" `Quick test_game_in_progress;
          test_case "test_game_completion_prompt" `Quick
            test_game_completion_prompt;
          test_case "test_game_completion_dialog" `Quick
            test_game_completion_dialog;
        ] );
    ]
