let hint_color = (0.5, 0.5, 0.5) (* gray color for hints *)

(* Draws a single hint number within a specific position in a Sudoku cell.

   ctxt: The Cairo drawing context x: The x-coordinate of the top-left corner of
   the cell y: The y-coordinate of the top-left corner of the cell n: The number
   to draw (1-9) i: The position index (0-8) within the 3x3 sub-grid inside a
   cell hint_size: The font size for the hint number *)
let draw_hint_number ctxt x y n i hint_size =
  (* Determine row (0, 1, or 2) within the 3x3 grid *)
  let hint_row = i / 3 in
  (* Determine column (0, 1, or 2) within the 3x3 grid *)
  let hint_col = i mod 3 in

  (* Calculate exact x, y coordinates for this hint number, 5. and 15. are the
     padding values *)
  let hint_x =
    x +. (float_of_int hint_col *. Configure_ui.cell_size /. 3.) +. 5.
  in
  let hint_y =
    y +. (float_of_int hint_row *. Configure_ui.cell_size /. 3.) +. 15.
  in

  (* Position the drawing cursor *)
  Cairo.move_to ctxt hint_x hint_y ;

  (* Set the font size for the hint (smaller than regular numbers) *)
  Cairo.set_font_size ctxt (float_of_int hint_size) ;

  (* Draw the hint number *)
  Cairo.show_text ctxt (string_of_int n)

(* Draws all hint numbers on the Sudoku board.

   ctxt: The Cairo drawing context hints: A 9x9 array where each element
   contains a list of valid numbers for that cell *)
let draw_hints ctxt (hints : int list array array) =
  (* Make hints much smaller - 20% of cell size *)
  let hint_size = int_of_float (Configure_ui.cell_size *. 0.2) in

  (* Set hint color from predefined hint_color *)
  let r, g, b = hint_color in
  Cairo.set_source_rgb ctxt r g b ;

  (* Set font face to Sans with normal weight *)
  Cairo.select_font_face ctxt "Sans" ~weight:Cairo.Normal ;

  (* Iterate through each cell in the 9x9 hints array *)
  Array.iteri
    (fun row row_array ->
      Array.iteri
        (fun col hints ->
          (* Only draw hints if there are valid numbers for this cell *)
          if hints <> []
          then
            (* Calculate the top-left position of this cell *)
            let x =
              Configure_ui.margin +. (float_of_int col *. Configure_ui.cell_size)
            in
            let y =
              Configure_ui.margin +. (float_of_int row *. Configure_ui.cell_size)
            in

            (* Draw each hint number in its position within the cell *)
            List.iteri
              (fun i n -> draw_hint_number ctxt x y n i hint_size)
              hints)
        row_array)
    hints

(* A reusable empty hints array to avoid creating new ones each time *)
let empty_hints = Array.make_matrix 9 9 []

(* Creates a copy of the empty hints array *)
let create_empty_hints () = Array.map (fun row -> Array.copy row) empty_hints

(* Creates an empty hint array with no hints shown. *)
let clear_all_hints () = empty_hints

(* Calculates all valid moves for every empty cell on the board. *)
let compute_all_hints board =
  let hints = create_empty_hints () in
  for row = 0 to 8 do
    for col = 0 to 8 do
      (* Only compute hints for empty, non-fixed cells *)
      if
        Validation_board.is_empty board.(row).(col)
        && not (Validation_board.is_fixed board.(row).(col))
      then hints.(row).(col) <- Solve.get_valid_numbers board ~row ~col
    done
  done ;
  hints

(* Filters the hints array to only show hints for empty, non-fixed cells.
   Optimized to work in-place when possible. *)
let filter_hints original_board (hints_board : int list array array) :
    int list array array =
  Array.iteri
    (fun row row_array ->
      Array.iteri
        (fun col _value ->
          (* Clear hints for non-empty or fixed cells *)
          if
            (not (Validation_board.is_empty original_board.(row).(col)))
            || Validation_board.is_fixed original_board.(row).(col)
          then hints_board.(row).(col) <- [])
        row_array)
    hints_board ;
  hints_board
