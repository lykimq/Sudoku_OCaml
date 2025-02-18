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
      | Empty -> check_box (r + 1) (c + 1)
      | (Fixed n | Mutable n) when n = value -> true
      | _ -> check_box (r + 1) (c + 1)
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
        List.init 9 (fun i -> i + 1)
        |> List.filter (fun value -> is_valid_move board ~row ~col ~value)

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
