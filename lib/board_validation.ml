open Board

let value_in_cells board coords ~value =
  List.exists
    (fun (r, c) ->
      match board.(r).(c) with
      | Empty -> false
      | (Fixed n | Mutable n) when n = value -> true
      | _ -> false)
    coords

(* Helper function to check if a number exists in given range *)
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
