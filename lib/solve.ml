open Board

let value_in_row board ~row ~value =
  let rec check col =
    if col >= 9
    then false
    else
      match board.(row).(col) with
      | Empty -> check (col + 1)
      | (Fixed n | Mutable n) when n = value -> true
      | _ -> check (col + 1)
  in
  check 0

let value_in_col board ~col ~value =
  let rec check row =
    if row >= 9
    then false
    else
      match board.(row).(col) with
      | Empty -> check (row + 1)
      | (Fixed n | Mutable n) when n = value -> true
      | _ -> check (row + 1)
  in
  check 0

let value_in_box board ~row ~col ~value =
  let box_row = row / 3 * 3 in
  let box_col = col / 3 * 3 in
  let rec check_box r c =
    if r >= box_row + 3
    then false
    else if c >= box_col + 3
    then check_box (r + 1) box_col
    else
      match board.(r).(c) with
      | Empty -> check_box r (c + 1)
      | (Fixed n | Mutable n) when n = value -> true
      | _ -> check_box r (c + 1)
  in
  check_box box_row box_col

let is_valid_pos row col = row >= 0 && row < 9 && col >= 0 && col < 9

let is_valid_move board ~row ~col ~value =
  value >= 1 && value <= 9
  && (not (value_in_row board ~row ~value))
  && (not (value_in_col board ~col ~value))
  && not (value_in_box board ~row ~col ~value)

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
               (not (value_in_row board ~row ~value))
               && (not (value_in_col board ~col ~value))
               && not (value_in_box board ~row ~col ~value))

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

let is_board_solved board =
  let is_valid_set numbers =
    let sorted = List.sort compare numbers in
    sorted = [1; 2; 3; 4; 5; 6; 7; 8; 9]
  in

  (* Check all rows *)
  let check_rows () =
    let rec check_row row =
      if row >= 9
      then true
      else
        let numbers =
          Array.to_list
            (Array.map
               (function Empty -> 0 | Fixed n | Mutable n -> n)
               board.(row))
        in
        if is_valid_set numbers then check_row (row + 1) else false
    in
    check_row 0
  in

  (* Check all columns *)
  let check_columns () =
    let rec check_col col =
      if col >= 9
      then true
      else
        let numbers =
          List.init 9 (fun row ->
              match board.(row).(col) with
              | Empty -> 0
              | Fixed n | Mutable n -> n)
        in
        if is_valid_set numbers then check_col (col + 1) else false
    in
    check_col 0
  in

  (* Check all 3x3 boxes *)
  let check_boxes () =
    let rec check_box box_row box_col =
      if box_row >= 9
      then true
      else if box_col >= 9
      then check_box (box_row + 3) 0
      else
        let numbers =
          List.init 9 (fun i ->
              let row = box_row + (i / 3) in
              let col = box_col + (i mod 3) in
              match board.(row).(col) with
              | Empty -> 0
              | Fixed n | Mutable n -> n)
        in
        if is_valid_set numbers then check_box box_row (box_col + 3) else false
    in
    check_box 0 0
  in

  (* Board is solved if all conditions are met and no empty cells *)
  let no_empty_cells () =
    not (Array.exists (Array.exists (( = ) Empty)) board)
  in

  no_empty_cells () && check_rows () && check_columns () && check_boxes ()

let get_game_status board =
  if is_board_solved board
  then
    Some
      "Congratulations! You've solved the Sudoku puzzle correctly! Would you \
       like to start a new game?"
  else None
