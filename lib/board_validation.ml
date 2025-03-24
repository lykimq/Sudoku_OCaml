open Board

let is_valid_set_cells arr =
  let seen = Array.make 10 false in
  let valid = ref true in
  Array.iteri
    (fun _i cell ->
      match cell with
      | (Fixed n | Mutable n) when n >= 1 && n <= 9 ->
          if seen.(n) then valid := false else seen.(n) <- true
      | _ -> valid := false)
    arr ;
  !valid

let value_in_cells board coords ~value =
  List.exists
    (fun (r, c) ->
      match board.(r).(c) with
      | Empty -> false
      | (Fixed n | Mutable n) when n = value -> true
      | _ -> false)
    coords

let is_valid_row board row = is_valid_set_cells board.(row)

let is_valid_col board col =
  let col_arr = Array.init 9 (fun row -> board.(row).(col)) in
  is_valid_set_cells col_arr

let is_valid_box board box_idx =
  let start_row = box_idx / 3 * 3 in
  let start_col = box_idx mod 3 * 3 in
  let box_arr =
    Array.init 9 (fun i ->
        let r = start_row + (i / 3) in
        let c = start_col + (i mod 3) in
        board.(r).(c))
  in
  is_valid_set_cells box_arr

let value_in_row board ~row ~value =
  value_in_cells board (List.init 9 (fun c -> (row, c))) ~value

let value_in_col board ~col ~value =
  value_in_cells board (List.init 9 (fun r -> (r, col))) ~value

let value_in_box board ~row ~col ~value =
  let start_row = row / 3 * 3 in
  let start_col = col / 3 * 3 in
  value_in_cells board
    (List.init 9 (fun i -> (start_row + (i / 3), start_col + (i mod 3))))
    ~value
