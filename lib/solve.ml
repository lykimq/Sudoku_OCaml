open Board

let is_valid_pos row col = row >= 0 && row < 9 && col >= 0 && col < 9

let is_valid_move board ~row ~col ~value =
  value >= 1 && value <= 9
  && (not (Board.value_in_row board ~row ~value))
  && (not (Board.value_in_col board ~col ~value))
  && not (Board.value_in_box board ~row ~col ~value)

let get_valid_numbers board ~row ~col =
  if not (is_valid_pos row col)
  then []
  else
    match board.(row).(col) with
    | Fixed _ | Mutable _ -> []
    | Empty ->
        (* Generate list of numbers 1-9 and filter out any that appear in same
           row, column, or box *)
        List.init 9 (fun i -> i + 1)
        |> List.filter (fun value ->
               (not (Board.value_in_row board ~row ~value))
               && (not (Board.value_in_col board ~col ~value))
               && not (Board.value_in_box board ~row ~col ~value))

let get_all_hints board =
  let hints = Array.make_matrix 9 9 [] in
  for row = 0 to 8 do
    for col = 0 to 8 do
      hints.(row).(col) <- get_valid_numbers board ~row ~col
    done
  done ;
  hints

(* Clear a cell back to Empty *)
let clear_cell board ~row ~col =
  if not (is_valid_pos row col)
  then None
  else
    match board.(row).(col) with
    | Fixed _ -> None (* Cannot clear fixed cells *)
    | Empty | Mutable _ ->
        let new_board = Array.map Array.copy board in
        new_board.(row).(col) <- Empty ;
        Some new_board

let set_cell board ~row ~col ~value =
  if not (is_valid_pos row col)
  then None
  else
    match board.(row).(col) with
    | Fixed _ -> None (* Cannot set value in fixed cells *)
    | Empty | Mutable _ ->
        let new_board = Array.map Array.copy board in
        new_board.(row).(col) <- Mutable value ;
        if is_valid_move board ~row ~col ~value
        then (
          Board.clear_invalid ~row ~col ;
          Some (new_board, true))
        else (
          Board.mark_invalid ~row ~col ;
          Some (new_board, false))

let get_game_status board =
  if Board.is_board_solved board
  then
    Some
      "Congratulations! You've solved the Sudoku puzzle correctly! Would you \
       like to start a new game?"
  else None
