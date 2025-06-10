open Board

(* Creates empty hint storage structure matching board dimensions. *)
let make_empty_hint_board () = Array.make_matrix 9 9 []

(* Returns valid values (1-9) that can be placed at position according to Sudoku
   rules. *)
let get_valid_numbers board ~row ~col =
  match board.(row).(col) with
  | Fixed _ | Mutable _ -> []
  | Empty ->
      let coords = Board_validation.get_constraint_coords row col in
      let all_coords =
        List.concat [coords.row_coords; coords.col_coords; coords.box_coords]
      in
      List.init 9 (fun i -> i + 1)
      |> List.filter (fun value ->
             not (Board_validation.contains_value board all_coords value))

(* Calculates all possible valid moves for every empty cell. *)
let get_all_hints board =
  Array.init 9 (fun row ->
      Array.init 9 (fun col ->
          match board.(row).(col) with
          | Empty -> get_valid_numbers board ~row ~col
          | Fixed _ | Mutable _ -> []))
