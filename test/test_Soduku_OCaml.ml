open Soduku_OCaml
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
  check bool "Empty board" false (Board.is_board_solved board)

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
  check bool "Valid board solved" true (Board.is_board_solved board)

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
  check bool "Duplicate numbers in row" false (Board.is_board_solved board)

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
  check bool "Duplicate numbers in column" false (Board.is_board_solved board)

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
  check bool "Duplicate numbers in box" false (Board.is_board_solved board)

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
        ] );
    ]
