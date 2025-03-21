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
  let coords = List.init 9 (fun c -> (row, c)) in
  value_in_cells board coords ~value

let value_in_col board ~col ~value =
  let coords = List.init 9 (fun r -> (r, col)) in
  value_in_cells board coords ~value

let value_in_box board ~row ~col ~value =
  let box_row = row / 3 * 3 in
  let box_col = col / 3 * 3 in
  let coords =
    List.init 9 (fun i -> (box_row + (i / 3), box_col + (i mod 3)))
  in
  value_in_cells board coords ~value
